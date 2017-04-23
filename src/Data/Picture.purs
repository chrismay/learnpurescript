module Data.Picture where

import Prelude
import Data.Maybe(Maybe(..))
type Picture = Array Shape
data Shape = Circle Point Number
           |Rectangle Point Number  Number
           |Line Point Point
           |Text Point String

data Point = Point {x ::Number, y::Number}

showPoint :: Point -> String
showPoint (Point { x: x, y: y }) =
  "(" <> show x <> ", " <> show y <> ")"

instance showShape :: Show Shape where
  show (Circle c r)      = "Circle; c:" <> (showPoint c) <> ", r:"<> (show r)
  show (Rectangle c w h) = "Rectangle; c:"<> showPoint c <> ",w:" <> show w <> ",h" <> show h
  show (Line s e)  = "Line; s"<> showPoint s <> ",e:"<> showPoint e
  show (Text loc text) = "Text; c" <> showPoint loc <> ",t:"<> text

scale:: Shape->Shape
scale (Circle c r) = Circle (x2 c) (r*2.0)
scale (Rectangle c h w) =Rectangle (x2 c) (h*2.0) (w*2.0)
scale (Line s f)= Line (x2 s) (x2 f)
scale (Text c s) = Text (x2 c) s

x2::Point->Point
x2 (Point{x,y}) = Point {x:x*2.0, y:y*2.0}

text:: Shape->Maybe String
text (Text c s) = Just s
text _ = Nothing

showPicture:: Picture->Array String
showPicture = map show


data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }
--bounds :: Picture->Bounds
--bounds = foldl combine emptyBounds
  --where
    --combine :: Bounds -> Shape -> Bounds
    --combine b shape = shapeBounds shape \/ b
--
-----------
c::Shape
c = Circle (Point {x:3.0,y:2.0}) 5.0 
r::Shape
r = Rectangle (Point {x:1.0,y:4.0}) 4.5 2.0
p::Picture
p = [c, r]

