module Harambe.System.Graphics.Color (
  module Harambe.System.Graphics.Color,
  Color,
) where

import Harambe.System.Internal.Color

import qualified Graphics.Gloss as G


newtype Red = Red Float
newtype Green = Green Float
newtype Blue = Blue Float
newtype Alpha = Alpha Float

rgb :: Red -> Green -> Blue -> Color
rgb r g b =
    rgba r g b (Alpha 1.0)

rgba :: Red -> Green -> Blue -> Alpha -> Color
rgba (Red r) (Green g) (Blue b) (Alpha a) =
    GlossColor (G.makeColor r g b a)