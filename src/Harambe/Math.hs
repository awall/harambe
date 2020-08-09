module Harambe.Math
  ( Angle(..)
  , sin
  , cos
  , pointingAt
  ) where

import Harambe.System.Internal.Point

import Prelude hiding (sin, cos, atan2)
import qualified Prelude as P (sin, cos, atan2)


newtype Angle = Degrees Float


sin :: Angle -> Float
sin d = P.sin $ toRadians d

cos :: Angle -> Float
cos d = P.cos $ toRadians d

pointingAt :: Point -> Point -> Angle
pointingAt (x1,y1) (x2,y2) = toDegrees $ P.atan2 (y2-y1) (x2-x1)

toRadians :: Angle -> Float
toRadians (Degrees d) = d / 180 * pi

toDegrees :: Float -> Angle
toDegrees r = Degrees $ r * 180 / pi