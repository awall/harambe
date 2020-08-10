import Harambe.System
import Harambe.Math as M

import Control.Lens
import System.Random

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
  return
    $ scale (h, h)
    $ pictures $ 
      [ translate (game^.monster) $ color darkred $ circleSolid (Radius 0.06)
      , translate (game^.position) $ rotateCCW (game^.angle) $ pictures $
        [ color white $ rotateCCW (Degrees 20) $ polygon [(sw - 0.02, -0.03), (sw + 0.15, -0.03), (sw + 0.15, -0.04), (sw - 0.02, -0.04)]
        , color green $ circleSolid (Radius 0.05)
        ]
      , translate (game^.mouse) $ color red $ circleSolid (Radius 0.01)
      ]
  where sw = game^.sword  
    
handleEvent :: Event -> Game -> IO Game
handleEvent (EventButton KeyEsc _) _ = do
  _ <- exitSuccess
  error "Program terminated."

handleEvent (EventButton MouseLeft Press) g0 = do
  r <- randomIO
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
      KeyW -> move y forward
      KeyS -> move y backward
      KeyD -> move x forward
      KeyA -> move x backward      
      _ -> id
    mul = case pressRelease of Press -> 1.0; Release -> -1.0
    forward  a = a + mul
    backward a = a - mul
    move xy dir = over (velocity . xy) dir
    x = _1
    y = _2

handleEvent (EventMotion (mx,my)) g0 = do
  (_, h) <- getWindowSize
  let g1 = g0 & set mouse (mx/h,my/h)
  return g1

update :: Float -> Game -> IO Game
update seconds g0 = do
  let g1 = g0 & set angle ((g0^.position) `M.pointingAt` (g0^.mouse))
  let g2 = g1 & position %~ move
  let g3 = g2 & hop . _1 %~ (\t -> t - seconds)
  let g4 = g3 & sword %~ movesword
  let g5 = g4 & strike %~ (\t -> t - seconds)
  let g6 = g5 & monster %~ movemonster
  let g7 = g6 & set monsterAngle nextMonsterAngle
  return g7
  where
    (hopt,hopa) = g0^.hop
    (dx,dy) = g0^.velocity
    speed = 0.3 * seconds
    jspeed = 2.4 * min seconds hopt
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
      | mx > 0.5 = Degrees 180
      | my < -0.5 = Degrees 90
      | my > 0.5 = Degrees 270
      | otherwise = ma