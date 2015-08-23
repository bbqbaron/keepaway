module Keepaway where

import Char exposing (toCode)
import Color exposing (green, red)
import Debug exposing (log)
import Dict exposing (Dict, empty, get, insert, keys, toList, update, values)
import Graphics.Collage exposing (collage, filled, Form, group, move, outlined, solid, square, text, toForm)
import Graphics.Element exposing (Element, image)
import Html exposing (div, Html)
import Keyboard exposing (arrows, isDown, space)
import List exposing (drop, filter, filterMap, foldl, head, isEmpty, length, map, member, reverse, sort, sortBy)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Random exposing (customGenerator, generate, Generator, initialSeed, int, pair, Seed)
import Signal exposing ((<~), (~), dropRepeats, foldp, Mailbox, mailbox, mergeMany)
import Text exposing (fromString)

import Signal.Extra exposing (foldp')
import Signal.Time exposing (startTime)

import Astar exposing (movePoint, pickDir, prioritize)
import Const exposing (..)
import Types exposing (..)
import Util exposing (bound, cond, inBounds, origin)

updates : Mailbox Action
updates = mailbox Idle

-- generators
pointGenerator : Generator (Int,Int)
pointGenerator = pair (int 0 (height-1)) (int 0 (width-1))

hpGenerator = int 1 maxMonsterHp
valueGenerator = int 1 maxItemValue

classNumGenerator = int 0 ((length classes)-1)

classGenerator = customGenerator (\seed ->
        let (idx,s') = generate classNumGenerator seed
            class = drop idx classes |> head |> withDefault {name="???"}
        in (class,s')
    )

addItem : Maybe Item -> Maybe Square -> Maybe Square
addItem i ms = case ms of
    Just s -> Just {s|item<-i}
    Nothing -> Nothing

type alias SpecMaker a = List (Point,a) -> Seed -> (List (Point,a),Seed)
type alias Inserter a = a -> Maybe Square -> Maybe Square

generateItem : SpecMaker Int
generateItem list seed =
    let (p,s') = generate pointGenerator seed
        (val,s'') = generate valueGenerator s'
    in ((p,val)::list,s'')

generateMonster : SpecMaker Int
generateMonster list seed =
    let (p,s') = generate pointGenerator seed
        (hp,s'') = generate hpGenerator s'
    in ((p,hp)::list,s'')

generatePC : SpecMaker Class
generatePC list seed =
    let (p,s') = generate pointGenerator seed
        (class,s'') = generate classGenerator s'
    in ((p,class)::list,s'')

createItemIn : Inserter Int
createItemIn value square =
    case square of
        Just square' -> Just {square'|item<-Just {name="I", value=value}}
        Nothing -> Nothing

addMonster : Inserter Int
addMonster hp s = 
    case s of
        Just s' -> Just {s'|monster<-Just {name="M", hp=hp, cooldown=2, currentCooldown=0}}
        Nothing -> Nothing

addPC : Inserter Class
addPC class s = 
    case s of
        Just s' -> Just {s'|pc<-Just {
            name="PC", 
            class=class, 
            statuses=[],
            xp=0}
        }
        Nothing -> Nothing

addRandom : SpecMaker a -> Inserter a -> Int -> Model -> Model
addRandom genFn addFn howMany model =
    let grid = model.grid
        seed = model.seed
        (specs,s') = foldl (\_ (l,s) -> genFn l s) ([], seed) [0..howMany-1]
        grid' = foldl (\(p,a) g' -> update p (addFn a) g') grid specs
    in {model|
            grid<-grid',
            seed<-s'}

addItems = addRandom generateItem createItemIn numberOfItems
addMonsters = addRandom generateMonster addMonster numberOfMonsters
addPCs = addRandom generatePC addPC 2

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

swapItems : Model -> Model
swapItems model =
    let player = model.player
        grid = model.grid
        position = player.position
        carrying = player.carrying
        item = case get position grid of
            Just {item} -> item
            Nothing -> Nothing
        player' = {player|carrying<-item}
        grid' = update position (addItem carrying) grid
    in {
        model|
            player<-player',
            grid<-grid'
    }

m : (a->b) -> Maybe a -> Maybe b
m fn a = case a of
    Just i -> Just (fn i)
    Nothing -> Nothing

setPC : Maybe PC -> Square -> Square
setPC pc s = {s|pc<-pc}

isStun a = case a of
    Stun n -> True
    _ -> False

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
                grid' = update (y,x) (m (setPC Nothing)) grid
            in update dest (m (setPC (Just pc))) grid'

movePCFrom : Point -> Grid -> Grid
movePCFrom p grid =
    let pc = get p grid |> withDefault emptySquare |> (.pc)
    in 
        case pc of
            Just pc' -> movePCFrom' p pc' grid
            Nothing -> grid

damage : Monster -> Maybe Monster
damage monster =
    let hp' = monster.hp - 1
    in cond 
        (hp'>0) 
        (Just {monster|hp<-hp'})
        Nothing

unjust : Maybe (Maybe a) -> Maybe a
unjust m = case m of
    Just m' -> m'
    Nothing -> Nothing

resolveCollisions : Model -> Model
resolveCollisions model =
    let grid = model.grid
        resolve = \(y,x) s ->
            case s.pc of
                Just pc ->
                    -- TODO having to unwrap a double Maybe seems wrong
                    let monster' = m damage s.monster |> unjust
                    -- TODO seems redundant to check item existence twice
                        getXp = s.item /= Nothing || (s.monster /= Nothing && monster' == Nothing)
                        value = case s.item of
                            Just i -> i.value
                            Nothing -> 0
                        xp' = pc.xp + (cond getXp value 0)
                        pc' = {pc|xp<-xp'}
                    in {s|
                        item<-Nothing, 
                        monster<-monster',
                        pc<-Just pc'
                    }
                Nothing -> s
    in {model|grid<-Dict.map resolve grid}

calculatePoints : Model -> Model
calculatePoints model =
    let grid = model.grid
        accPoints = \{item} points ->
            let new = 
                case item of
                    Just i -> i.value
                    Nothing -> 0
            in new + points
        points = grid |> values |> foldl accPoints 0
        points' = points + (case player.carrying of
                                Just i -> i.value
                                Nothing -> 0)
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
        decStun stun = case stun of
            Stun n ->
                if | n > 1 -> (n-1) |> Stun |> Just
                   | otherwise -> Nothing
            _ -> Nothing
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
            case monster of
                Just m -> 
                    if member p neighbors then Just (p,m) else Nothing
                Nothing -> Nothing)

-- TODO dedupe with filterSquares
getWith : Grid -> (Square->Maybe a) -> List (Point,a)
getWith grid fn =
    grid
        |> toList
        |> filterMap (\(p,s) -> case fn s of
                Just i -> Just (p,i)
                Nothing -> Nothing)

filterSquares : Grid -> (Square->Maybe a) -> List (Point,Square)
filterSquares grid fn =
    grid
        |> toList
        |> filter (\(p,s) -> case fn s of
                Just i -> True
                Nothing -> False)

processAOOs : Model -> Model
processAOOs model =
    let grid = model.grid
        pcPoints = getWith grid (.pc) |> map fst
        triggerAOOsOn : Point -> PC -> Grid
        triggerAOOsOn p pc =
            let monsters = getMonstersNextTo grid p |> log "wtf"
            in
                case head monsters of
                    Just (mP,m) ->
                        if m.currentCooldown == 0 then
                            let statuses' = pc.statuses ++ [Stun 12]
                                pc' = {pc|statuses<-statuses'}
                                monster' = {m|currentCooldown<-m.cooldown}
                            in grid
                                |> update p (\s -> case s of
                                    Just s' -> Just {s'|pc<-Just pc'}
                                    Nothing -> Nothing)
                                |> update mP (\s -> case s of
                                    Just s' -> Just {s'|monster<-Just monster'}
                                    Nothing -> Nothing)
                        else
                            grid
                    Nothing -> grid
        triggerAOOs : Point -> Grid -> Grid
        triggerAOOs p grid =
            let s = get p grid
            in
                case s of
                    Just s' -> 
                        case s'.pc of
                            Just pc -> triggerAOOsOn p pc
                            Nothing -> grid
                    Nothing -> grid
        grid' = foldl triggerAOOs grid pcPoints
    in {model|grid<-grid'}

tickCooldowns : Model -> Model
tickCooldowns model =
    let grid = model.grid
        monsterPoints = filterSquares grid (.monster)
        cooldownAt : (Point,Square) -> (Point,Square)
        cooldownAt (point, square) =
            case square.monster of
                Just monster ->
                    let cc' = max 0 (monster.currentCooldown-1)
                        m' = {monster|currentCooldown<-cc'}
                    in (point, {square|monster<-Just m'})
                Nothing -> (point, square)
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

startTimeSeed : Signal Seed
startTimeSeed = initialSeed << round <~ startTime

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

flatten : List (List a) -> List a
flatten l = foldl (\subl accl -> foldl (::) accl subl) [] l

toPos : Int -> Int -> (Float, Float)
toPos y x = 
    let (oY, oX) = origin
    in (oY - (toFloat (y*tileSize)), oX + (toFloat (x*tileSize)))

renderPlayer : Player -> Form
renderPlayer {carrying, position} = 
    let (y,x) = position
        base = getImage "Imp"
        withText = case carrying of
                Just i -> group [base, i |> toString |> fromString |> text]
                Nothing -> base
        in withText |> move (toPos y x)

getImage : String -> Form
getImage name = "assets/art/"++name++".png"
    |> image tileSize tileSize
    |> toForm

renderSquare : Int -> Int -> Square -> Form
renderSquare y x {item, monster, pc} = 
    let ground = getImage "Ground"
        form = 
        case pc of
            Just pc' -> getImage pc'.class.name |> Just
            Nothing -> 
                case monster of
                    Just monster' -> getImage "Goblin" |> Just
                    Nothing -> 
                        case item of
                            Just n -> getImage "Gold" |> Just
                            Nothing -> Nothing
        grp = case form of
            Just f -> group [ground,f]
            Nothing -> ground
    in grp |> move (toPos y x)

renderRow : Int -> Grid -> List Form
renderRow y g = map (\x->get (y,x) g |> withDefault emptySquare |> renderSquare y x) xRange

addForm : Form -> List Form -> List Form
addForm f l = l ++ [f]

renderGrid : Model -> Element
renderGrid {grid, player} =
    -- TODO this is a relic of a nested list.
    -- just `Dict.map` over `grid`?
    map (\y->renderRow y grid) yRange
        |> flatten
        |> addForm (renderPlayer player)
        |> collage (height*tileSize) (width*tileSize)

renderPoints : Model -> Html
renderPoints model = model.player.points |> toString |> Html.text

render : Model -> Html
render model =
    if model.player.points > 0 then
        div [] [
            renderGrid model |> Html.fromElement,
            renderPoints model
        ]
    else
        div [] [
            Html.text "DEAD"
        ]

main : Signal Html
main = render <~ state