module Keepaway where

import Color exposing (green, red)
import Debug exposing (log)
import Dict exposing (Dict, empty, get, insert, keys, toList, update, values)
import Html exposing (Html)
import Keyboard exposing (arrows, isDown, space)
import List exposing (drop, filter, filterMap, foldl, head, length, map, member, reverse)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Random exposing (Seed)
import Signal exposing ((<~), (~), foldp, Mailbox, mailbox, mergeMany)

import Signal.Extra exposing (foldp')

import Astar exposing (movePoint, pickDir, prioritize)
import Const exposing (..)
import Generators exposing (..)
import Grid exposing (..)
import Render exposing (render)
import Types exposing (..)
import Util exposing (bound, cond, inBounds)

updates : Mailbox Action
updates = mailbox Idle

init : (a,Seed) -> Model
init (_,s) = {
        player={
                alive=False,
                carrying=Nothing,
                points=0,
                position=(0,0)
            },
        seed=s,
        grid=foldl (\y g -> foldl (\x g'-> insert (y,x) emptySquare g') g xRange) empty yRange
    } 
        |> addItems
        |> addMonsters
        |> addPCs
        -- who knows? pc could be standing on an item
        |> resolveCollisions

start : Model -> Model
start model =
    let player = model.player
        player' = {player|alive<-True}
    in {model|player<-player'}

step : (Action, Seed) -> Model -> Model
step (action, _) model = 
    let model' = 
        case action of
            Move dir -> 
                let player = model.player
                    (y,x) = player.position
                    (y',x') = dirToPoint dir
                    player' = {player|position<-bound (y+y', x+x')}
                    model' = tick model
                in {model'|player<-player'}
                    |> movePCs
                    |> processAOOs
                    |> tickCooldowns
                    |> calculatePoints
                    |> squashPlayer
                    |> maybeEndGame
            Fetch -> swapItems model
            Restart ->
                cond (model.player.alive) model ((init ((), model.seed)) |> start)
            _ -> model
    in model'

onRelease : Action -> Signal Bool -> Signal Action
onRelease a k =
    k
        |> foldp (\n o -> if o == Just Idle && not n then Just a else Just Idle) Nothing
        |> Signal.map (withDefault Idle)

state : Signal Model
state = 
        (,) <~ (mergeMany
            [
                updates.signal,
                Signal.filter ((/=) Idle) Idle (dirsToAction <~ arrows),
                space |> onRelease Fetch,
                isDown 82 |> onRelease Restart
            ]
        ) ~ startTimeSeed
    |> foldp' step init

main : Signal Html
main = render <~ state