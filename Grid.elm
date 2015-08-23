module Grid where

{-| Just to avoid further overloading "Map"

-}

import Dict exposing (get, insert, keys, toList, update, values)
import List exposing (any, filter, filterMap, foldl, head, map, member)
import Maybe exposing (andThen, Maybe(..), withDefault)

import Astar exposing (pickDir)
import Types exposing (Ability(..), Action(..), Dir(..), emptySquare, Grid, Item, Model, Monster, PC, Point, Square, StatusType(..))
import Util exposing (cond, inBounds, isStun)

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

stunned : PC -> Bool
stunned {statuses} = any isStun statuses

movePCFrom' : Point -> Grid -> PC -> Grid
movePCFrom' (y,x) grid pc =
    cond (stunned pc)
        grid
        (let current = get (y,x) grid |> withDefault emptySquare
             stuck = current.monster /= Nothing
             dest = pickDir grid (y,x) |> cond stuck (y,x)
             grid' = update (y,x) (Maybe.map (setPC Nothing)) grid
        in   update dest (Maybe.map (setPC (Just pc))) grid')

movePCFrom : Point -> Grid -> Grid
movePCFrom point grid =
    get point grid `andThen` (.pc)
        |> Maybe.map (movePCFrom' point grid)
        |> withDefault grid

damageMonster : Monster -> Maybe Monster
damageMonster monster =
    let hp' = monster.hp - 1
    in cond 
        (hp'>0) 
        (Just {monster|hp<-hp'})
        Nothing

resolveCollisionAt : Point -> Square -> Square
resolveCollisionAt _ square =
    let resolve' : PC -> Square
        resolve' pc = 
            let monster' = square.monster `andThen` damageMonster
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
    in {model|grid<-Dict.map resolveCollisionAt grid}

calculatePoints : Model -> Model
calculatePoints model =
    let grid = model.grid
        accPoints {item} points =
            let new = Maybe.map (.value) item |> withDefault 0
            in new + points
        points = grid |> values |> foldl accPoints model.player.points
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
        player' = cond hasPC {player|alive<-False} player
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

isTough : PC -> Bool
isTough {class} = any ((==) Tough) class.abilities

processAOOs : Model -> Model
processAOOs model =
    let grid = model.grid
        triggerAOOsOn' : Grid -> Point -> PC -> Grid
        triggerAOOsOn' grid p pc =
            if stunned pc then
                grid
            else
                let monsters = getMonstersNextTo grid p
                        |> filter (snd>>(.currentCooldown)>>(==) 0)
                    processMonster : Grid -> (Point,Monster) -> Grid
                    processMonster grid (p2,m) =
                        if m.currentCooldown == 0 then
                            let statuses' = pc.statuses ++ [{
                                    statusType=Stun, 
                                    duration=cond (isTough pc) 2 3}]
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
    let tickMonster monster = {monster|currentCooldown<-max 0 (monster.currentCooldown-1)}
    in  {model|grid<-Dict.map (\_ square -> {square|monster<-Maybe.map tickMonster square.monster}) model.grid}

maybeEndGame : Model -> Model
maybeEndGame model =
    let noItems = (model.grid |> values |> any ((.item)>> (/=) Nothing) |> not)
    in
        cond noItems
        (let player = model.player
             player' = {player|alive<-False}
        in {model|player<-player'})
        model
