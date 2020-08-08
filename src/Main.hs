import Harambe.System

import Data.IORef


main :: IO ()
main = do
  position <- newIORef (0,0)
  v <- newIORef 0
  let game = Game { position = position, v = v }
  runEventLoop "Harambe RPG" game renderIO handleIO updateIO


data Game = Game {
  position :: IORef Point,
  v :: IORef Float
}

renderIO :: Game -> IO Picture
renderIO game = do
  (w,h) <- getWindowSize
  (xy) <- readIORef (position game)
  return $ scale (h,h) $ translate xy $ color turquoise $ circleSolid (Radius 0.05)

handleIO :: Event -> Game -> IO Game
handleIO (EventKey KeyEsc _) game = do
  exitSuccess
  return game

handleIO (EventKey KeyW Down) game = do
  modifyIORef (v game) (\x -> x + 1.0)
  return game
handleIO (EventKey KeyW Up) game = do
  modifyIORef (v game) (\x -> x - 1.0)
  return game
handleIO (EventKey KeyS Down) game = do
  modifyIORef (v game) (\x -> x - 1.0)
  return game
handleIO (EventKey KeyS Up) game = do
  modifyIORef (v game) (\x -> x + 1.0)
  return game

handleIO event game = do
  return game

updateIO :: Float -> Game -> IO Game
updateIO seconds game = do
  vvel <- readIORef (v game)
  modifyIORef (position game) (\(x,y) -> (x, y + vvel * 0.2 * seconds))
  return game

turquoise = rgba (Red 0.0) (Green 1.0) (Blue 1.0) (Alpha 0.5)