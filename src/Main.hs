import Harambe.System
import Harambe.Math as M

import Control.Lens hiding (Identity)
import System.Random (randomIO)

data Game = Game 
  { _position :: Point
  , _velocity :: (Float, Float)
  , _mouse :: Point
  , _angle :: Angle
  , _hop :: (Float, Angle) -- seconds remaining
  , _strike :: Float -- seconds remaining
  , _sword :: Float -- distance pushed forward
  , _monster :: Point
  , _monsterAngle :: Angle
  , _hits :: Int
  }
$(makeLenses ''Game)


main :: IO ()
main = do
  let game = Game {
      _position = (0,0)
    , _velocity = (0,0)
    , _mouse = (0,0)
    , _angle = Degrees 0
    , _hop = (0.0, Degrees 0)
    , _strike = 0.0
    , _sword = 0.0
    , _monster = (0.3, 0.3)
    , _monsterAngle = Degrees 270
    , _hits = 0
    }
  runEventLoop "Harambe RPG" game render handleEvent update

white, green, red, darkred :: Color
white   = rgb (Red 1.0) (Green 1.0) (Blue 1.0)
red     = rgb (Red 1.0) (Green 0.0) (Blue 0.0)
green   = rgb (Red 0.0) (Green 1.0) (Blue 0.0)
darkred = rgb (Red 0.5) (Green 0.0) (Blue 0.0)

render :: Game -> IO Picture
render game = do
  (_, h) <- getWindowSize
  return $ pictures 
    [ color white $ text (show (game^.hits))
    , scale (h, h)
      $ pictures
        [ translate (game^.monster) $ color darkred $ circleSolid (Radius 0.06)
        , translate (game^.position) $ rotateCCW (game^.angle) $ color green $ circleSolid (Radius 0.05)
        , swordPicture game
        , translate (game^.mouse) $ color red $ circleSolid (Radius 0.01)
        , translate (swordHitbox game) $ color green $ circleSolid (Radius 0.01)
      ]
    ]
    
handleEvent :: Event -> Game -> IO Game
handleEvent (EventButton KeyEsc _) _ = do
  _ <- exitSuccess
  error "Program terminated."

handleEvent (EventButton MouseLeft Press) g0 = do
  r :: Int <- randomIO
  let anng = Degrees $ fromIntegral (r `mod` 360)
  let g1 = g0 & strike %~ nextStrike
  let g2 = g1 & set monsterAngle anng
  return g2
  where
    nextStrike t
      | t <= 0.0  = 0.1
      | otherwise = t

handleEvent (EventButton KeySpace Press) game =  
  return $ game & hop %~ nextHop
  where
    nextHop (t,a)
      | t <= 0.0  = (0.1, game^.angle)
      | otherwise = (t, a)

handleEvent (EventButton key pressRelease) game =
  return $ next game
  where 
    next = case key of
      KeyW -> move up
      KeyS -> move down
      KeyD -> move right
      KeyA -> move left
      _ -> id
    mul = case pressRelease of Press -> 1.0; Release -> -1.0
    up       = (0.0, mul)
    down     = (0.0, -mul)
    left     = (-mul, 0.0)
    right    = (mul, 0.0)
    move dir = over velocity (transformz $ Translate dir Identity)

handleEvent (EventMotion (mx,my)) g0 = do
  (_, h) <- getWindowSize
  let g1 = g0 & set mouse (mx/h,my/h)
  return g1

update :: Float -> Game -> IO Game
update seconds g0 = do
  let g1 = g0 & hits %~ addIfHit
  let g2 = g1 & set angle ((g0^.position) `M.pointingAt` (g0^.mouse))
  let g3 = g2 & position %~ move
  let g4 = g3 & hop . _1 %~ (\t -> t - seconds)
  let g5 = g4 & sword %~ movesword
  let g6 = g5 & strike %~ (\t -> t - seconds)
  let g7 = g6 & monster %~ movemonster
  let g8 = g7 & set monsterAngle nextMonsterAngle
  return g8
  where
    (hopt,hopa) = g0^.hop    
    speed = 0.3 * seconds
    jspeed = 2.4 * min seconds hopt
    (dx,dy) = g0^.velocity
    move (x,y)
      | hopt > 0.0 = (x - jspeed * M.cos hopa, y - jspeed * M.sin hopa)
      | otherwise  = (x +  speed * dx,         y +  speed * dy)
    st = 0.5 * (g0^.strike)
    movesword s
      | st >= -0.05 = s + st
      | otherwise = 0
    ma = g0^.monsterAngle
    mspeed = speed * 0.5
    movemonster (x,y) = (x + mspeed * M.cos ma, y + mspeed * M.sin ma)
    (mx,my) = g0^.monster
    nextMonsterAngle 
      | mx < -0.5 = Degrees 0
      | mx >  0.5 = Degrees 180
      | my < -0.5 = Degrees 90
      | my >  0.5 = Degrees 270
      | otherwise = ma
    addIfHit x
      | g0^.strike > 0.0 && overlaps (swordHitbox g0) (g0^.monster) = x+1
      | otherwise = x

overlaps :: Point -> Point -> Bool
overlaps (x1,y1) (x2,y2) =
  distance < 0.07
  where distance = sqrt ((x2-x1)**2 + (y2-y1)**2)

swordTransform :: Game -> Transform      
swordTransform game =
  Translate (game^.position)
    $ Rotate (game^.angle)
    $ Rotate (Degrees 20)
    $ Translate (game^.sword, 0.0)
    $ Identity

swordHitbox :: Game -> (Float, Float)
swordHitbox game =
  transformz (swordTransform game) (0.15, -0.035)

swordPicture :: Game -> Picture
swordPicture game =
  color white
    $ transformz (swordTransform game)
    $ polygon [(-0.02, -0.03), (0.15, -0.03), (0.15, -0.04), (-0.02, -0.04)]