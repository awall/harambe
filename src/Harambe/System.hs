module Harambe.System
  ( module Harambe.System.Event
  , module Harambe.System.Graphics
  , runEventLoop
  , exitSuccess
  , exitFailure
  ) where

import Harambe.System.Event
import Harambe.System.Graphics
import Harambe.System.Internal.Picture

import System.Exit (exitSuccess, exitFailure)
import Control.Monad (liftM)

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as G


runEventLoop 
  :: String               -- ^ window title
  -> a                    -- ^ the world 
  -> (a -> IO Picture)    -- ^ render the world
  -> (Event -> a -> IO a) -- ^ update world in response to event + mouse position
  -> (Float -> a -> IO a) -- ^ update world on every timestep
  -> IO ()
runEventLoop title state render react advance =
  G.playIO display G.black fps state render' react' advance'
  where
    fps = 60
    display = G.InWindow title (500, 500) (100, 100)
    render' = liftM pictureToGloss . render
    advance' s a = advance s a
    pictureToGloss (GlossPicture p) = p
    react' e a = case eventFromGloss e of
      Just ne -> react ne a
      Nothing -> return a

eventFromGloss :: G.Event -> Maybe Event
eventFromGloss (G.EventKey k ud _ _) = do
  b <- button
  Just $ EventButton b pressRelease
  where 
    pressRelease = case ud of 
      G.Down -> Press
      G.Up   -> Release
    button = case k of 
      G.Char c | c >= 'a' && c <= 'z' -> Just $ toEnum $ (fromEnum KeyA) + (fromEnum c - fromEnum 'a')
      G.SpecialKey  G.KeySpace        -> Just KeySpace
      G.SpecialKey  G.KeyEsc          -> Just KeyEsc
      G.MouseButton G.LeftButton      -> Just MouseLeft
      G.MouseButton G.RightButton     -> Just MouseRight
      _ -> Nothing

eventFromGloss (G.EventMotion xy) = 
  Just $ EventMotion xy

eventFromGloss _ =
  Nothing