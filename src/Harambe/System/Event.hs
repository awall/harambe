module Harambe.System.Event
  ( Button(..)
  , Event(..)
  , PressRelease(..)
  ) where

import Harambe.System.Internal.Point


data Event
  = EventButton Button PressRelease
  | EventMotion Point

data PressRelease = Press | Release

data Button
  = KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | KeySpace
  | KeyEsc
  | MouseLeft
  | MouseRight
  deriving (Enum)