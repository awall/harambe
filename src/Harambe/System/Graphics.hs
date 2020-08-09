module Harambe.System.Graphics(
  module Harambe.System.Graphics,
  module Harambe.System.Graphics.Color,
  Picture,
  Point,
  Pixels,
) where

import Harambe.Math(Angle(..))
import Harambe.System.Graphics.Color
import Harambe.System.Internal.Color
import Harambe.System.Internal.Point
import Harambe.System.Internal.Picture

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Environment as G
import qualified Graphics.UI.GLUT.Window as GL
import qualified Graphics.Rendering.OpenGL.GL as GL


type Path = [Point]
newtype Radius = Radius Pixels 
newtype Thickness = Thickness Pixels

getWindowSize :: IO Point
getWindowSize = do 
  GL.Size x y <- GL.get GL.windowSize
  return (fromIntegral x, fromIntegral y)

blank :: Picture
blank = GlossPicture $ G.blank

polygon :: Path -> Picture
polygon path = GlossPicture $ G.polygon path

line :: Path -> Picture
line path = GlossPicture $ G.line path

circle :: Radius -> Picture
circle = thickCircle (Thickness 0)

thickCircle :: Thickness -> Radius -> Picture
thickCircle (Thickness t) (Radius r) =
  GlossPicture $ G.thickCircle t r

circleSolid :: Radius -> Picture
circleSolid (Radius r) =
  GlossPicture $ G.circleSolid r

arc :: Angle -> Angle -> Radius -> Picture
arc = thickArc (Thickness 0)

thickArc :: Thickness -> Angle -> Angle -> Radius -> Picture
thickArc (Thickness t) (Degrees start) (Degrees stop) (Radius r) =
  GlossPicture $ G.thickArc t start stop r

arcSolid :: Angle -> Angle -> Radius -> Picture
arcSolid (Degrees start) (Degrees stop) (Radius r) =
  GlossPicture $ G.arcSolid start stop r

color :: Color -> Picture -> Picture
color (GlossColor c) (GlossPicture p) =
  GlossPicture $ G.color c p

translate :: Point -> Picture -> Picture
translate (x, y) (GlossPicture p) =
  GlossPicture $ G.translate x y p

rotate :: Angle -> Picture -> Picture
rotate (Degrees d) (GlossPicture p) =
  GlossPicture $ G.rotate d p

scale :: (Float, Float) -> Picture -> Picture
scale (x, y) (GlossPicture p) =
  GlossPicture $ G.scale x y p
  
pictures :: [Picture] -> Picture
pictures ps =
  GlossPicture $ G.pictures $ map (\(GlossPicture p) -> p) ps

lineLoop :: Path -> Picture
lineLoop p =
  GlossPicture $ G.lineLoop p