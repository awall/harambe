import Harambe.System


main :: IO ()
main = do
  eventLoop "Harambe RPG" Game renderIO handleIO updateIO

data Game = Game

renderIO :: Game -> IO Picture
renderIO game = do
  (x, y) <- getWindowSize
  return $ render (x, y) game

handleIO :: Event -> Game -> IO Game
handleIO (EventKey KeyQ) _ = 
  quit

handleIO event game = do
  return game

updateIO :: Float -> Game -> IO Game
updateIO seconds game = do
  return $ update seconds game

update :: Float -> Game -> Game
update seconds game = game

render :: Point -> Game -> Picture
render (x, y) game = 
  color turquoise $ circleSolid (Radius (y/2))

turquoise = rgb (Red 0.0) (Green 1.0) (Blue 1.0)