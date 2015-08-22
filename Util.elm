module Util where

import Const exposing (height, tileSize, width)
import Types exposing (Point)

bound : Point -> Point
bound (y,x) = (max 0 (min y (height-1)), max 0 (min x (width-1)))

cond : Bool -> a -> a -> a
cond bool a b = if bool then a else b

origin : (Float,Float)
origin = (tileSize * (height / 2 - 0.5), tileSize * (width / 2 * -1 + 0.5))