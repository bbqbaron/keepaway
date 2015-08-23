module Util where

import List exposing (foldl)

import Const exposing (height, tileSize, width)
import Types exposing (Point, Status, StatusType(..))

bound : Point -> Point
bound (y,x) = (max 0 (min y (height-1)), max 0 (min x (width-1)))

cond : Bool -> a -> a -> a
cond bool a b = if bool then a else b

flatten : List (List a) -> List a
flatten l = foldl (\subl accl -> foldl (::) accl subl) [] l

inBounds : Point -> Bool
inBounds (y,x) = 
    let (y',x') = bound (y,x)
    in  y==y' && x==x'

origin : (Float,Float)
origin = (tileSize * (height / 2 - 0.5), tileSize * (width / 2 * -1 + 0.5))

isStun : Status -> Bool
isStun = (.statusType) >> (==) Stun