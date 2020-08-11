module Harambe.Math
  ( Angle(..)
  , Transform(..), Transformable(..)
  , sin, cos
  , pointingAt
  ) where

import Harambe.System.Internal.Point

import Prelude hiding (sin, cos, atan2)
import qualified Prelude as P (sin, cos, atan2)


newtype Angle = Degrees Float

data Transform
  = Identity
  | Rotate    Angle          Transform
  | Translate (Float, Float) Transform
  | Scale     (Float, Float) Transform

class Transformable t where
  transformz :: Transform -> t -> t

instance Transformable (Float, Float) where
  transformz Identity            = id
  transformz (Translate (a,b) t) = (\(x,y) -> (a+x,b+y))                              . transformz t
  transformz (Scale     (a,b) t) = (\(x,y) -> (a*x,b*y))                              . transformz t
  transformz (Rotate d t)        = (\(x,y) -> (x*cos d - y*sin d, x*sin d + y*cos d)) . transformz t


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