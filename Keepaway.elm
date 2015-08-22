module Keepaway where

import Char exposing (toCode)
import Color exposing (green, red)
import Debug exposing (log)
import Dict exposing (Dict, empty, get, insert, keys, update)
import Graphics.Collage exposing (collage, filled, Form, group, move, outlined, solid, square, text)
import Graphics.Element exposing (Element)
import Html exposing (div, Html)
import Keyboard exposing (arrows, isDown, space)
import List exposing (filter, foldl, head, length, map, reverse, sort, sortBy)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Signal exposing ((<~), dropRepeats, foldp, Mailbox, mailbox, mergeMany)
import Text exposing (fromString)

import Astar exposing (movePoint, pickDir, prioritize)
import Const exposing (height, tileSize, width)
import Types exposing (..)
import Util exposing (bound, cond, origin)

updates : Mailbox Action
updates = mailbox Idle

yRange : List Int
yRange = [0..height-1]

xRange : List Int
xRange = [0..width-1]

addItem : Maybe Item -> Maybe Square -> Maybe Square
addItem i ms = case ms of
    Just s -> Just {s|item<-i}
    Nothing -> Nothing

addItems : Grid -> Grid
addItems grid =
    let items = [((2,3),3),((5,0),6),((2,3),7),((5,1),4),((6,3),2)]
        addItem value s =
            case s of
                Just s' -> Just {s'|item<-Just {name="I", value=value}}
                Nothing -> Nothing
    in foldl (\(p,value) g' -> update p (addItem value) g') grid items

addPCs : Grid -> Grid
addPCs grid = update 
    (7,7) 
    (\s->case s of
        Just s' -> Just {s'|pc<-Just {name="F", xp=0}}
        Nothing -> Nothing)
    grid

addMonsters : Grid -> Grid
addMonsters grid =
    let monsters = [((3,1),4), ((4,4),3), ((5,2),6), ((7,0),3)]
        addMonster hp s = 
            case s of
                Just s' -> Just {s'|monster<-Just {name="M", hp=hp}}
                Nothing -> Nothing
    in foldl (\(p,hp) g'-> update p (addMonster hp) g') grid monsters

init : Model
init = {
        player={
                alive=True,
                carrying = Nothing,
                points=0,
                position = (0,0)
            },
        grid = foldl (\y g -> foldl (\x g'-> insert (y,x) emptySquare g') g xRange) empty yRange
            |> addItems
            |> addPCs
            |> addMonsters
            -- who knows? pc could be standing on an item
            |> resolveCollisions
    } |> calculatePoints

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

movePCFrom : Point -> Grid -> Grid
movePCFrom (y,x) grid =
    let current = get (y,x) grid |> withDefault emptySquare
        stuck = current.monster /= Nothing
        dest = pickDir grid (y,x) |> cond stuck (y,x)
        pc = get (y,x) grid |> withDefault emptySquare |> (.pc)
        grid' = update (y,x) (m (setPC Nothing)) grid
        grid'' = update dest (m (setPC pc)) grid'
    in grid''

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

resolveCollisions : Grid -> Grid
resolveCollisions grid =
    let resolve = \(y,x) s ->
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
    in Dict.map resolve grid

calculatePoints : Model -> Model
calculatePoints model =
    let grid = model.grid
        accPoints = \_ {item} points ->
            let new = 
                case item of
                    Just i -> i.value
                    Nothing -> 0
            in new + points
        points = Dict.foldl accPoints 0 grid
        player = model.player
        player' = {player|points<-points}
    in {model|player<-player'}

movePCs : Model -> Model
movePCs model = 
    let grid = model.grid
        pcs = Dict.filter (\_ {pc} -> pc /= Nothing) grid |> keys
        grid' = foldl movePCFrom grid pcs |> resolveCollisions
    in {model|grid<-grid'}

squashPlayer : Model -> Model
squashPlayer model =
    let player = model.player
        pos = player.position
        square = get pos model.grid |> withDefault emptySquare
        hasPC = square.pc /= Nothing
        player' = cond hasPC {player|points<-0} player
    in {model|player<-player'}

step : Action -> Model -> Model
step action model = 
    let model' = 
        case action of
            Move dir -> 
                let player = model.player
                    (y,x) = player.position
                    (y',x') = dirToPoint dir
                    player' = {player|position<-bound (y+y', x+x')}
                in {model|player<-player'}
                    |> movePCs
            Fetch -> swapItems model
            Restart -> cond (model.player.points<=0) init model
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
state = foldp step init 
    (mergeMany 
        [
            updates.signal,
            (dirsToAction <~ arrows),
            space |> onRelease Fetch,
            isDown 82 |> onRelease Restart
        ]
    )

flatten : List (List a) -> List a
flatten l = foldl (\subl accl -> foldl (::) accl subl) [] l

toPos : Int -> Int -> (Float, Float)
toPos y x = 
    let (oY, oX) = origin
    in (oY - (toFloat (y*tileSize)), oX + (toFloat (x*tileSize)))

renderPlayer : Player -> Form
renderPlayer {carrying, position} = 
    let (y,x) = position
        base = filled green (square tileSize)
        withText = case carrying of
                Just i -> group [base, i |> toString |> fromString |> text]
                Nothing -> base
        in withText |> move (toPos y x)

renderSquare : Int -> Int -> Square -> Form
renderSquare y x {item, monster, pc} = 
    let form = 
        case pc of
            Just pc' ->
                pc'.name ++ ": " ++ (toString pc'.xp) 
                    |> fromString 
                    |> text
            Nothing -> 
                case monster of
                    Just monster' ->
                        monster'.name ++ ": " ++ (toString monster'.hp) 
                            |> fromString 
                            |> text
                    Nothing -> 
                        case item of
                            Just n -> n.name |> toString |> fromString |> text
                            Nothing -> outlined (solid red) (square tileSize)
    in form |> move (toPos y x)

renderRow : Int -> Grid -> List Form
renderRow y g = map (\x->get (y,x) g |> withDefault emptySquare |> renderSquare y x) xRange

addForm : Form -> List Form -> List Form
addForm f l = l ++ [f]

renderGrid : Model -> Element
renderGrid {grid, player} =
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