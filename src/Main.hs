import Harambe.System

import Control.Lens
import Control.Lens.TH
import Data.IORef


data Game = Game {
  _position :: Point,
  _velocity :: (Float, Float)
}
$(makeLenses ''Game)

main :: IO ()
main = do
  let game = Game { _position = (0, 0), _velocity = (0, 0) }
  runEventLoop "Harambe RPG" game renderIO handleIO updateIO





renderIO :: Game -> IO Picture
renderIO game = do
  (w, h) <- getWindowSize
  return
    $ scale (h, h)
    $ translate (game^.position)
    $ color turquoise
    $ circleSolid (Radius 0.05)
  
handleIO :: Event -> Game -> IO Game
handleIO (EventKey KeyEsc _) game = do
  exitSuccess
  return game

handleIO (EventKey key upOrDown) game =
  return (next game)
  where 
    mul = case upOrDown of Up -> -1.0; Down -> 1.0
    forward a = a + mul
    backward a = a - mul
    move xy dir = over (velocity . xy) dir
    x = _1
    y = _2
    next = case key of
      KeyW -> move y forward
      KeyS -> move y backward
      KeyD -> move x forward
      KeyA -> move x backward
      _ -> id
    
handleIO event game = do
  return game

updateIO :: Float -> Game -> IO Game
updateIO seconds game = do
  return (game & position %~ move)
  where
    (dx, dy) = game^.velocity
    speed = 0.3 * seconds
    move (x, y) = (x + dx*speed, y + dy*speed)

turquoise :: Color
turquoise = rgba (Red 0.0) (Green 1.0) (Blue 1.0) (Alpha 0.5)