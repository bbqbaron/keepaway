module Keepaway where

import Color exposing (green, red)
import Debug exposing (log)
import Dict exposing (Dict, empty, get, insert, keys, toList, update, values)
import Html exposing (Html)
import Keyboard exposing (arrows, isDown, space)
import List exposing (drop, filter, filterMap, foldl, head, length, map, member, reverse)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Random exposing (Seed)
import Signal exposing ((<~), (~), dropRepeats, foldp, Mailbox, mailbox, mergeMany)

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

port playSound : Signal String
port playSound = (.soundToPlay) <~ state

init : (a,Seed) -> Model
init (_,s) = {
        player={
            alive=False,
            carrying=Nothing,
            points=0,
            position=(4,4)
        },
        seed=s,
        grid=foldl (\y g -> foldl (\x g'-> insert (y,x) emptySquare g') g xRange) empty yRange,
        soundToPlay=""
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
    let silentModel = {model|soundToPlay <- ""}
        actionedModel = 
        case action of
            Move dir -> 
                let player = silentModel.player
                    (y,x) = player.position
                    (y',x') = dirToPoint dir
                    player' = {player|position<-bound (y+y', x+x')}
                    tickedModel = tick silentModel
                in {tickedModel|player<-player'}
                    |> movePCs
                    |> resolveCollisions
                    |> processAOOs
                    |> tickCooldowns
                    |> calculatePoints
                    |> squashPlayer
                    |> maybeEndGame
            Fetch -> swapItems silentModel
            Restart ->
                cond (silentModel.player.alive) silentModel (init ((), silentModel.seed) |> start)
            _ -> silentModel
    in actionedModel

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