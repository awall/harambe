module Harambe.System(
    module Harambe.System.Event,
    module Harambe.System.Graphics,
    eventLoop,
    quit,
)
where

import Harambe.System.Event
import Harambe.System.Graphics
import Harambe.System.Private.Picture

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Exit(exitSuccess)
import Control.Monad(liftM)


quit :: IO a
quit = exitSuccess

eventLoop :: String -> a -> (a -> IO Picture) -> (Event -> a -> IO a) -> (Float -> a -> IO a) -> IO ()
eventLoop title state render react advance =
  G.playIO display G.black fps state render' react' advance
  where
    fps = 60
    display = G.InWindow title (500, 500) (100, 100)
    react' e a = react (eventFromGloss e) a
    render' = liftM pictureToGloss . render
    pictureToGloss (GlossPicture p) = p

eventFromGloss :: G.Event -> Event
eventFromGloss (G.EventKey (G.Char c) _ _ _) | c >= 'a' && c <= 'z' =
  EventKey $ toEnum $ (fromEnum KeyA) + (fromEnum c - fromEnum 'a')

eventFromGloss (G.EventKey _ _ _ _) =
  EventKey KeyOther

eventFromGloss _ =
  EventOther