module Util where

import Const exposing (height, width)
import Types exposing (Point)

bound : Point -> Point
bound (y,x) = (max 0 (min y (height-1)), max 0 (min x (width-1)))

cond : Bool -> a -> a -> a
cond bool a b = if bool then a else b