import Harambe.System
import Harambe.Math as M

import Control.Lens


data Game = Game 
  { _position :: Point
  , _velocity :: (Float, Float)
  , _mouse :: Point
  , _angle :: Angle
  , _hop :: (Float, Angle) -- seconds
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
    }
  runEventLoop "Harambe RPG" game render handleEvent update


white, green, red :: Color
white = rgb (Red 1.0) (Green 1.0) (Blue 1.0)
red   = rgb (Red 1.0) (Green 0.0) (Blue 0.0)
green = rgb (Red 0.0) (Green 1.0) (Blue 0.0)


render :: Game -> IO Picture
render game = do
  (_, h) <- getWindowSize
  return
    $ scale (h, h)
    $ pictures [
      translate (game^.position) $ rotateCCW (game^.angle) $ pictures [
        color green $ circleSolid (Radius 0.05),
        color white $ polygon [(0.0, -0.03), (0.12, -0.03), (0.12, -0.04), (0.0, -0.04)]
      ],
      translate (game^.mouse) $ color red $ circleSolid (Radius 0.01)
    ]

    
handleEvent :: Event -> Game -> IO Game
handleEvent (EventButton KeyEsc _) _ = do
  _ <- exitSuccess
  error "Program terminated."

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
  return g3
  where
    (hopt,hopa) = g0^.hop
    (dx,dy) = g0^.velocity
    speed = 0.3 * seconds
    jspeed = 2.4 * min seconds hopt
    move (x,y)
      | hopt > 0.0 = (x - jspeed * M.cos hopa, y - jspeed * M.sin hopa)
      | otherwise  = (x +  speed * dx,         y +  speed * dy)