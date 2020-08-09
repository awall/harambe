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
  -> (Event -> a -> IO a) -- ^ update world in response to event
  -> (Float -> a -> IO a) -- ^ update world on every timestep
  -> IO ()
runEventLoop title state render react advance = do
  let react' e a = react (eventFromGloss e) a
  G.playIO display G.black fps state render' react' advance'
  where
    fps = 60
    display = G.InWindow title (500, 500) (100, 100)
    render' = liftM pictureToGloss . render
    advance' s a = advance s a
    pictureToGloss (GlossPicture p) = p

eventFromGloss :: G.Event -> Event
eventFromGloss (G.EventKey k ud _ _) =
  EventKey key upDown
  where 
    upDown = case ud of 
      G.Down -> Down
      G.Up   -> Up
    key = case k of 
      G.Char c | c >= 'a' && c <= 'z' -> toEnum $ (fromEnum KeyA) + (fromEnum c - fromEnum 'a')
      G.SpecialKey G.KeySpace -> KeySpace
      G.SpecialKey G.KeyEsc   -> KeyEsc
      _ -> KeyOther

eventFromGloss (G.EventMotion xy) = 
  EventMotion xy

eventFromGloss _ =
  EventOther