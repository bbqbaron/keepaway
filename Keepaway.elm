module Keepaway where

import Color exposing (green, red)
import Debug exposing (log)
import Dict exposing (Dict, empty, get, insert, keys, toList, update, values)
import Html exposing (Html)
import Keyboard exposing (arrows, isDown, space)
import List exposing (drop, filter, filterMap, foldl, head, isEmpty, length, map, member, reverse)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Random exposing (Seed)
import Signal exposing ((<~), (~), foldp, Mailbox, mailbox, mergeMany)

import Signal.Extra exposing (foldp')

import Astar exposing (movePoint, pickDir, prioritize)
import Const exposing (..)
import Generators exposing (..)
import Render exposing (render)
import Types exposing (..)
import Util exposing (bound, cond, inBounds, isStun)

updates : Mailbox Action
updates = mailbox Idle

init : (a,Seed) -> Model
init (_,s) = {
        player={
                alive=True,
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
        |> calculatePoints

dirsToAction : {x:Int, y:Int} -> Action
dirsToAction {x,y} = 
    if | x == -1 -> Move Left
       | x == 1 -> Move Right
       | y == -1 -> Move Down
       | y == 1 -> Move Up
       | otherwise -> Idle

dirToPoint : Dir -> Point
dirToPoint d = 
    case d of
        Left -> (1,0)
        Right -> (-1,0)
        Up -> (0,1)
        Down -> (0,-1)
        _ -> (0,0)

addItem : Maybe Item -> Maybe Square -> Maybe Square
addItem i ms = Maybe.map (\s->{s|item<-i}) ms

swapItems : Model -> Model
swapItems model =
    let player = model.player
        grid = model.grid
        position = player.position
        carrying = player.carrying
        item = get position grid `andThen` (.item)
        player' = {player|carrying<-item}
        grid' = update position (addItem carrying) grid
    in {
        model|
            player<-player',
            grid<-grid'
    }

setPC : Maybe PC -> Square -> Square
setPC pc s = {s|pc<-pc}

movePCFrom' : Point -> PC -> Grid -> Grid
movePCFrom' (y,x) pc grid =
    let stunned = filter isStun pc.statuses |> isEmpty |> not
    in 
        if stunned then
            grid
        else
            let current = get (y,x) grid |> withDefault emptySquare
                stuck = current.monster /= Nothing
                dest = pickDir grid (y,x) |> cond stuck (y,x)
                grid' = update (y,x) (Maybe.map (setPC Nothing)) grid
            in update dest (Maybe.map (setPC (Just pc))) grid'

movePCFrom : Point -> Grid -> Grid
movePCFrom point grid =
    get point grid `andThen` (.pc)
        |> Maybe.map (\pc -> movePCFrom' point pc grid)
        |> withDefault grid

damage : Monster -> Maybe Monster
damage monster =
    let hp' = monster.hp - 1
    in cond 
        (hp'>0) 
        (Just {monster|hp<-hp'})
        Nothing

resolve : Point -> Square -> Square
resolve _ square =
    let resolve' : PC -> Square
        resolve' pc = 
            let monster' = square.monster `andThen` damage
                getXp = square.item /= Nothing || (square.monster /= Nothing && monster' == Nothing)
                value : Int
                value = Maybe.map (.value) square.item |> withDefault 0
                xp' = pc.xp + (cond getXp value 0)
                pc' = {pc|xp<-xp'}
            in {square|
                    item<-Nothing,
                    monster<-monster',
                    pc<-Just pc'
                }
    in Maybe.map resolve' square.pc 
        |> withDefault square

resolveCollisions : Model -> Model
resolveCollisions model =
    let grid = model.grid
    in {model|grid<-Dict.map resolve grid}

calculatePoints : Model -> Model
calculatePoints model =
    let grid = model.grid
        accPoints = \{item} points ->
            let new = Maybe.map (.value) item |> withDefault 0
            in new + points
        points = grid |> values |> foldl accPoints 0
        points' = points + (Maybe.map (.value) player.carrying |> withDefault 0)
        player = model.player
        player' = {player|points<-points'}
    in {model|player<-player'}

movePCs : Model -> Model
movePCs model = 
    let grid = model.grid
        pcs = Dict.filter (\_ {pc} -> pc /= Nothing) grid |> keys
        grid' = foldl movePCFrom grid pcs
    in {model|grid<-grid'} |> resolveCollisions

tick : Model -> Model
tick model =
    let grid = model.grid
        decStun stun = 
            if | stun.duration > 1 -> Just {stun|duration<-stun.duration-1}
               | otherwise -> Nothing
        tickStun : PC -> PC
        tickStun pc =
            let stuns = filter isStun pc.statuses
                stuns' = filterMap decStun stuns
                nonstuns = filter (isStun>>not) pc.statuses
            in {pc|statuses<-nonstuns++stuns'}
        tick p s = case s.pc of
            Just pc -> {s|pc<-tickStun pc |> Just}
            Nothing -> s
        grid' = Dict.map tick grid
    in {model|grid<-grid'}

squashPlayer : Model -> Model
squashPlayer model =
    let player = model.player
        pos = player.position
        square = get pos model.grid |> withDefault emptySquare
        hasPC = square.pc /= Nothing
        player' = cond hasPC {player|points<-0} player
    in {model|player<-player'}

getNeighbors : Point -> List Point
getNeighbors (y,x) =
    let dirs = [(-1,0),(-1,-1),(-1,1),(0,1),(0,-1),(1,1),(1,0),(1,-1)]
    in map (\(y',x')->(y+y',x+x')) dirs

getMonstersNextTo : Grid -> Point -> List (Point,Monster)
getMonstersNextTo grid p =
    let neighbors = getNeighbors p |> filter inBounds
    in grid
        |> toList
        |> filterMap (\(p,{monster}) -> 
            cond (member p neighbors) (Maybe.map ((,) p) monster) Nothing)

getPCPoints : Grid -> List Point
getPCPoints grid =
    grid
        |> Dict.filter (\k {pc} -> pc /= Nothing)
        |> keys

filterSquares : Grid -> (Square->Maybe a) -> List (Point,Square)
filterSquares grid fn =
    grid
        |> toList
        |> filter (\(p,s) -> fn s /= Nothing)

processAOOs : Model -> Model
processAOOs model =
    let grid = model.grid
        triggerAOOsOn' : Grid -> Point -> PC -> Grid
        triggerAOOsOn' grid p pc =
            let monsters = getMonstersNextTo grid p
                    |> filter (snd>>(.currentCooldown)>>(==) 0)
                processMonster : Grid -> (Point,Monster) -> Grid
                processMonster grid (p2,m) =
                    if m.currentCooldown == 0 then
                        let statuses' = pc.statuses ++ [{statusType=Stun, duration=12}]
                            pc' = {pc|statuses<-statuses'}
                            monster' = {m|currentCooldown<-m.cooldown}
                        in grid
                            |> update p (Maybe.map (\s -> {s|pc<-Just pc'}))
                            |> update p2 (Maybe.map (\s -> {s|monster<-Just monster'}))
                    else grid
            in
                head monsters
                    |> Maybe.map (processMonster grid)
                    |> withDefault grid
        triggerAOOsOn : Point -> Grid -> Grid
        triggerAOOsOn p grid =
            get p grid `andThen` (.pc)
                |> Maybe.map (triggerAOOsOn' grid p)
                |> withDefault grid
        -- TODO just foldl over grid
        pcPoints = getPCPoints grid
        grid' = foldl triggerAOOsOn grid pcPoints
    in {model|grid<-grid'}

tickCooldowns : Model -> Model
tickCooldowns model =
    let grid = model.grid
        monsterPoints = filterSquares grid (.monster)
        cooldownAt : (Point,Square) -> (Point,Square)
        cooldownAt (point, square) =
            square.monster
                |> Maybe.map (\monster ->
                        let cc' = max 0 (monster.currentCooldown-1)
                            m' = {monster|currentCooldown<-cc'}
                        in (point, {square|monster<-Just m'}))
                |> withDefault (point, square)
        grid' = map cooldownAt monsterPoints
            |> foldl (uncurry insert) grid
    in {model|grid<-grid'}

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
            Fetch -> swapItems model
            Restart -> cond (model.player.points<=0) (init ((), model.seed)) model
            _ -> model
    in model'
        |> calculatePoints
        |> squashPlayer

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
                (dirsToAction <~ arrows),
                space |> onRelease Fetch,
                isDown 82 |> onRelease Restart
            ]
        ) ~ startTimeSeed
    |> foldp' step init

main : Signal Html
main = render <~ state