module Astar where

import Debug exposing (log)
import Dict exposing (get, keys)
import List exposing (filter, head, length, map, reverse, sort, sortBy, sum)
import Maybe exposing (Maybe(..), withDefault)

import Types exposing (..)
import Util exposing (bound, cond)

-- not really Astar; optimistic module naming
prioritize : Grid -> Point -> (Int, Point)
prioritize grid p =
    let priority = 
            get p grid
                -- TODO it's fine to drop priority,
                -- but since PCs cannot actually stack,
                -- this is a bug waiting to happen
                |> Maybe.map (\{item, monster, pc} -> 
                    withDefault 0 (Maybe.map (.value) item)
                    + withDefault 0 (Maybe.map ((.hp) >> (*) -1) monster)
                    + withDefault 0 (Maybe.map (\_ -> -1000) pc)
                )
                |> withDefault 0
    in (priority, p)

movePoint : Point -> Point -> Point
movePoint (y,x) (y1,x1) = (y+y1, x+x1)

pickDir : Grid -> Point -> Point
pickDir grid (y,x) =
    -- TODO cache and recalc if you're there
    let (y',x') = seek grid (y,x)
        (y'',x'') =
            if | y' > y -> (1,0)
               | y' < y -> (-1,0)
               | x' > x -> (0,1)
               | x' < x -> (0,-1)
               | otherwise -> (0,0)
    in movePoint (y,x) (y'',x'')
        |> bound

distance : Point -> Point -> (Int, Point)
distance (y1,x1) (y2,x2) =
    let dist = 
        [(y1-y2), (x1-x2)]
            |> map abs
            |> sum
    in (dist, (y2,x2))

seek : Grid -> Point -> Point
seek grid (y,x) =
    -- my goodness this is long
    let hasAnything _ {item} = item /= Nothing
        pointsOfInterest = Dict.filter hasAnything grid
        byDistance = 
            pointsOfInterest
                |> keys
                |> map (distance (y,x))
                |> sortBy fst
        shortestDistance = head byDistance |> withDefault (0,(0,0)) |> fst
        closest = filter (fst >> (==) shortestDistance) byDistance
        prioritized =
            closest
                |> map snd
                |> map (prioritize pointsOfInterest)
                |> sortBy fst
                |> reverse
                |> map snd
    in head prioritized |> withDefault (y,x)