module Keepaway where

import Color exposing (green, red)
import Debug exposing (log)
import Dict exposing (Dict, empty, get, insert, keys, update)
import Graphics.Collage exposing (collage, filled, Form, group, move, outlined, solid, square, text)
import Graphics.Element exposing (Element)
import Html exposing (div, Html)
import Keyboard exposing (arrows, space)
import List exposing (filter, foldl, head, map)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Signal exposing ((<~), dropRepeats, foldp, Mailbox, mailbox, mergeMany)
import Text exposing (fromString)

type Dir = Left|Down|Up|Right

type alias Point = (Int,Int)

type Action = Idle|Fetch|Move Dir

type alias Item = {name:String, value:Int}
type alias Monster = {name:String}
type alias PC = {name:String, xp:Int}

type alias Square = {
        item: Maybe Item,
        monster: Maybe Monster,
        pc: Maybe PC
    }

type alias Player = {
        carrying: Maybe Item,
        position: Point,
        points: Int
    }

type alias Grid = Dict Point Square

type alias Model = {
        grid: Grid,
        player: Player
    }

updates : Mailbox Action
updates = mailbox Idle

tileSize = 75
height = 8
width = 8
origin = (tileSize * (height / 2 - 0.5), tileSize * (width / 2 * -1 + 0.5))

emptySquare : Square
emptySquare = {item=Just {name="I", value=0}, monster=Nothing, pc=Nothing}

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
    let makeItem y x = Just {name=toString (y*10+x), value=1}
    in foldl (\y g->foldl (\x g'-> update (y,x) (addItem (makeItem y x)) g') g xRange) grid yRange

addPCs : Grid -> Grid
addPCs grid = update 
    (7,7) 
    (\s->case s of
        Just s' -> Just {s'|pc<-Just {name="F", xp=0}}
        Nothing -> Nothing)
    grid

addMonsters : Grid -> Grid
addMonsters grid =
    update
        (7,6)
        (\s->case s of
                Just s' -> Just {s'|monster<-Just {name="M"}}
                Nothing -> Nothing)
        grid

init : Model
init = {
        player={
                carrying = Nothing,
                points=0,
                position = (0,0)
            },
        grid = foldl (\y g -> foldl (\x g'-> insert (y,x) emptySquare g') g xRange) empty yRange
            |> addItems
            |> addPCs
            |> addMonsters
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

bound : Point -> Point
bound (y,x) = (max 0 (min y (height-1)), max 0 (min x (width-1)))

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

hasBooty : Grid -> Point -> Bool
hasBooty grid (y,x) =
    case get (y,x) grid of
        Just {item, monster} -> item /= Nothing || monster /= Nothing
        Nothing -> False

movePoint : Point -> Point -> Point
movePoint (y,x) (y1,x1) = (y+y1, x+x1)

pickDir : Grid -> Point -> Point
pickDir grid (y,x) =
    let dirs = [(1,0),(-1,0),(0,1),(0,-1)]
        dests = map (movePoint (y,x)) dirs
        inBounds = map bound dests |> filter ((/=) (y,x))
        candidates = filter (hasBooty grid) inBounds
    in head candidates |> withDefault (y,x)

movePCFrom : Point -> Grid -> Grid
movePCFrom (y,x) grid =
    let dest = pickDir grid (y,x)
        pc = get (y,x) grid |> withDefault emptySquare |> (.pc)
        grid' = update (y,x) (m (setPC Nothing)) grid
        grid'' = update dest (m (setPC pc)) grid'
    in grid''

cond : Bool -> a -> a -> a
cond bool a b = if bool then a else b

resolveCollisions : Grid -> Grid
resolveCollisions grid =
    let resolve = \(y,x) s ->
            case s.pc of
                Just pc ->
                    let getXp = s.item /= Nothing || s.monster /= Nothing
                        xp' = pc.xp + (cond getXp 1 0)
                        pc' = {pc|xp<-xp'}
                    in {s|
                        item<-Nothing, 
                        monster<-Nothing,
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

step : Action -> Model -> Model
step action model = case action of
    Move dir -> 
        let player = model.player
            (y,x) = player.position
            (y',x') = dirToPoint dir
            player' = {player|position<-bound (y+y', x+x')}
        in {model|player<-player'}
            |> movePCs
            |> calculatePoints
    Fetch -> swapItems model
    _ -> model

state : Signal Model
state = foldp step init 
    (mergeMany 
        [
            updates.signal,
            (dirsToAction <~ arrows),
            space
                |> foldp (\n o -> if o == Just Idle && not n then Just Fetch else Just Idle) Nothing
                |> Signal.map (withDefault Idle)
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
                        monster'.name |> fromString |> text
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
    div [] [
        renderGrid model |> Html.fromElement,
        renderPoints model
    ]

main : Signal Html
main = render <~ state