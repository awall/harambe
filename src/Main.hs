module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact(Event(..), Key(..))
import Graphics.Gloss.Interface.IO.Game(playIO)
import Graphics.Gloss.Interface.Environment(getScreenSize)
import Graphics.Gloss.Data.ViewPort

fps = 60 :: Int
width = 300 :: Int
height = 300 :: Int
offset = 100 :: Int

main :: IO ()
main = playIO window background fps initialState renderIO handleKeysIO updateIO

renderIO :: PongGame -> IO Picture
renderIO game = do
  (x, y) <- getScreenSize
  return $ render game

handleKeysIO :: Event -> PongGame -> IO PongGame
handleKeysIO event game = do
  return $ handleKeys event game

updateIO :: Float -> PongGame -> IO PongGame
updateIO seconds game = do
  return $ update seconds game

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds = wallBounce . moveBall seconds

-- A data structure to hold the state of the Pong game.
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Right player paddle height.
  } deriving Show 

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (-15, 0)
  , player1 = 40
  , player2 = -80
  }

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures [ball, walls,
                    mkPaddle rose 120 (-20),
                    mkPaddle orange (-120) 40]
  where
    --  The pong ball.
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10
    (vx, vy) = ballVel game
    vy' = if wallCollision (ballLoc game) radius
             then -vy
             else  vy

type Radius = Float 
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') _ _ _) game =
  game { ballLoc = (0, 0) }
handleKeys (EventKey (Char 'w') _ _ _) game =
  game { player1 = (player1 game) - 5 }
handleKeys (EventKey (Char 'a') _ _ _) game =
  game { player1 = (player1 game) + 5 }


-- Do nothing for all other events.
handleKeys _ game = game