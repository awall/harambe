module Harambe.System.Internal.Picture
  ( Picture(..)
  ) where

import qualified Graphics.Gloss as G


newtype Picture = GlossPicture G.Picture