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
            grid<-grid',
            soundToPlay<-cond (player.carrying /= Nothing) "drop" "coin"
    }

stunned : PC -> Bool
stunned {statuses} = any isStun statuses

removePC : PC -> Square -> Square
removePC pc square = {square|pcs <- filter ((/=) pc) square.pcs}

addPC : PC -> Square -> Square
addPC pc square = {square|pcs <- pc :: square.pcs}

movePCFrom : Point -> PC -> Grid -> Grid
movePCFrom (y,x) pc grid =
    cond (stunned pc)
        grid
        (let stuck = get (y,x) grid
                        |> Maybe.map ((.monster) >> (/=) Nothing)
                        |> withDefault False
             dest = pickDir grid (y,x) |> cond stuck (y,x)
             grid' = update (y,x) (Maybe.map (removePC pc)) grid
        in   update dest (Maybe.map (addPC pc)) grid')

movePCsFrom : (Point,Square) -> Grid -> Grid
movePCsFrom (point,square) grid =
    foldl (movePCFrom point) grid square.pcs

-- TODO better to preselect keys with PCs in them and foldl over them?
-- Could cut down on allocations.
movePCs : Model -> Model
movePCs model = 
    let grid = model.grid
        grid' = foldl movePCsFrom grid (toList grid)
    in {model|grid<-grid'}

damageMonster : Monster -> Maybe Monster
damageMonster monster =
    let hp' = monster.currentHP - 1
    in cond 
        (hp'>0) 
        (Just {monster|currentHP<-hp'})
        Nothing

resolveCollisionAt : Point -> Square -> Square
resolveCollisionAt _ square =
    let resolve' : PC -> Square -> Square
        resolve' pc square = 
            let others : List PC
                others = filter ((/=) pc) square.pcs
                monster' : Maybe Monster
                monster' = square.monster `andThen` damageMonster
                getXp : Bool
                getXp = square.item /= Nothing || (square.monster /= Nothing && monster' == Nothing)
                value : Int
                value = Maybe.map (.value) square.item |> withDefault 0
                xp' : Int
                xp' = pc.xp + (cond getXp value 0)
                pc' : PC
                pc' = {pc|xp<-xp'}
            in {square|
                    item<-Nothing,
                    monster<-monster',
                    pcs<-pc'::others
                }
    in foldl resolve' square square.pcs

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
        tick p s = {s|pcs<-map tickStun s.pcs}
        grid' = Dict.map tick grid
    in {model|grid<-grid'}

squashPlayer : Model -> Model
squashPlayer model =
    let player = model.player
        pos = player.position
        square = get pos model.grid |> withDefault emptySquare
        hasPC = square.pcs /= []
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
        |> Dict.filter (\_ {pcs} -> pcs /= [])
        |> keys

isTough : PC -> Bool
isTough {class} = any ((==) Tough) class.abilities

triggerMonsterAOOOn : Grid -> (Point,PC) -> (Point,Monster) -> Grid
triggerMonsterAOOOn grid (pcPoint,pc) (monsterPoint,monster) =
    if monster.currentCooldown == 0 then
        let statuses' = pc.statuses ++ [{
                statusType=Stun, 
                duration=cond (isTough pc) 2 3}]
            pc' = {pc|statuses<-statuses'}
            monster' = {monster|currentCooldown<-monster.cooldown}
        in grid
            |> update pcPoint (Maybe.map (\s -> 
                {s|pcs <- pc' :: (filter ((/=) pc) s.pcs)})
            )
            |> update monsterPoint (Maybe.map (\s -> {s|monster<-Just monster'}))
    else grid

triggerAOOsOnPC : Point -> Square -> Grid -> PC -> Grid
triggerAOOsOnPC point square grid pc =
    cond
        (stunned pc)
        grid
        (
            let monsters = getMonstersNextTo grid point |> filter (snd>>(.currentCooldown)>>(==) 0) 
            in
                head monsters
                    |> Maybe.map (triggerMonsterAOOOn grid (point,pc))
                    |> withDefault grid
        )

triggerAOOsAt : (Point, Square) -> Grid -> Grid
triggerAOOsAt (point, square) grid =
    square.pcs
        |> head
        |> Maybe.map (triggerAOOsOnPC point square grid)
        |> withDefault grid

processAOOs : Model -> Model
processAOOs model = {model|grid <- foldl triggerAOOsAt model.grid (toList model.grid)}

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
