module Harambe.System
  ( runLoop
  , Event(..)
  , Key(..)
) where

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Interact as GI
import qualified Graphics.Gloss.Interface.IO.Game as G

data Key =
  KeyA |
  KeyB |
  KeyC |
  KeyD |
  KeyE |
  KeyF |
  KeyG |
  KeyH |
  KeyI |
  KeyJ |
  KeyK |
  KeyL |
  KeyM |
  KeyN |
  KeyO |
  KeyP |
  KeyQ |
  KeyR |
  KeyS |
  KeyT |
  KeyU |
  KeyV |
  KeyW |
  KeyX |
  KeyY |
  KeyZ |
  KeyOther
  deriving (Enum)

data Event = 
  EventKey Key |
  EventOther

fromGloss :: G.Event -> Event
fromGloss (G.EventKey (G.Char c) _ _ _) | c >= 'a' && c <= 'z' = EventKey $ toEnum $ (fromEnum KeyA) + (fromEnum c - fromEnum 'a')
fromGloss (G.EventKey _ _ _ _) = EventKey KeyOther
fromGloss _ = EventOther

runLoop :: a -> (a -> IO G.Picture) -> (Event -> a -> IO a) -> (Float -> a -> IO a) -> IO ()
runLoop state render react advance =
  G.playIO display G.black fps state render react' advance
  where
    fps = 60
    display = G.FullScreen
    react' = react . fromGloss