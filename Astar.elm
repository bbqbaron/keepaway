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
        case get p grid of
            Just {item, monster} -> 
                cond (item /= Nothing) 5 0
                    +
                cond (monster /= Nothing) -10 0
            Nothing -> 0
    in (priority, p)

movePoint : Point -> Point -> Point
movePoint (y,x) (y1,x1) = (y+y1, x+x1)

pickDir : Grid -> Point -> Point
pickDir grid (y,x) =
    -- TODO cache and recalc if you're there
    let (y',x') = seek grid (y,x) |> log "sought"
        (y'',x'') =
            if | y' > y -> (1,0)
               | y' < y -> (-1,0)
               | x' > x -> (0,1)
               | x' < x -> (0,-1)
               | otherwise -> (0,0)
    in movePoint (y,x) (y'',x'')
        |> bound
        |> log "dest"

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
    let _ = log "at" (y,x)
        hasAnything _ {item} = item /= Nothing
        pointsOfInterest = Dict.filter hasAnything grid |> log "poi"
        byDistance = 
            pointsOfInterest
                |> keys
                |> map (distance (y,x))
                |> sortBy fst
        shortestDistance = head byDistance |> withDefault (0,(0,0)) |> fst
        closest = filter (fst >> (==) shortestDistance) byDistance |> log "closest"
        prioritized =
            closest
                |> map snd
                |> map (prioritize pointsOfInterest)
                |> sortBy fst
                |> reverse
                |> map snd
    in head prioritized |> withDefault (y,x)