module Harambe.System.Event
where

import Harambe.System.Internal.Point


data Event = 
  EventKey Key UpDown |
  EventMotion Point |
  EventOther

data UpDown = Up | Down

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

  KeySpace |
  KeyEsc |

  KeyOther
    deriving (Enum)