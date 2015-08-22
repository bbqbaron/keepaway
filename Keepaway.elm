module Keepaway where

import Color exposing (..)
import Debug exposing (log)
import Dict exposing (Dict, empty, get, insert, keys, update)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Keyboard exposing (..)
import List exposing (..)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Signal exposing ((<~), dropRepeats, foldp, Mailbox, mailbox, mergeMany)
import Text exposing (fromString)

type Dir = Left|Down|Up|Right

type alias Point = (Int,Int)

type Action = Idle|Fetch|Move Dir

type alias Item = Int
type alias Monster = Int
type alias PC = String

type alias Square = {
        item: Maybe Item,
        monster: Maybe Monster,
        pc: Maybe PC
    }

type alias Player = {
        carrying: Maybe Item,
        position: Point
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
emptySquare = {item=Just 0, monster=Nothing, pc=Nothing}

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
    foldl (\y g->foldl (\x g'-> update (y,x) (addItem (Just (y*10+x))) g') g xRange) grid yRange

addPCs : Grid -> Grid
addPCs grid = update 
    (7,7) 
    (\s->case s of
        Just s -> Just {s|pc<-Just "F"}
        Nothing -> Nothing)
    grid

init : Model
init = {
        player={
                carrying = Nothing,
                position = (0,0)
            },
        grid = foldl (\y g -> foldl (\x g'-> insert (y,x) emptySquare g') g xRange) empty yRange
            |> addItems
            |> addPCs
    }

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

pickDir : Point -> Point
pickDir (y,x) =
    let canRight = x > 0
        canDown = y > 0
    in  if | canRight -> (y,x-1)
           | canDown -> (y-1,x)
           | otherwise -> (y,x)

movePCFrom : Point -> Grid -> Grid
movePCFrom (y,x) grid =
    let dest = pickDir (y,x)
        pc = get (y,x) grid |> withDefault emptySquare |> (.pc)
        grid' = update (y,x) (m (setPC Nothing)) grid
        grid'' = update dest (m (setPC pc)) grid'
    in grid''

movePCs : Model -> Model
movePCs model = 
    let grid = model.grid
        pcs = Dict.filter (\_ {pc} -> pc /= Nothing) grid |> keys
        grid' = foldl movePCFrom grid pcs
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

renderPC : Point -> PC -> Form
renderPC (y,x) pc = pc |> fromString |> text |> move (toPos y x)

renderSquare : Int -> Int -> Square -> Form
renderSquare y x {item, pc} = 
    let form = 
        case pc of
            Just pc ->
                pc |> fromString |> text
            Nothing -> 
                case item of
                    Just n -> n |> toString |> fromString |> text
                    Nothing -> outlined (solid red) (square tileSize)
    in form |> move (toPos y x)

renderRow : Int -> Grid -> List Form
renderRow y g = map (\x->get (y,x) g |> withDefault emptySquare |> renderSquare y x) xRange

addForm : Form -> List Form -> List Form
addForm f l = l ++ [f]

render : Model -> Element
render {grid, player} =
    map (\y->renderRow y grid) yRange
        |> flatten
        |> addForm (renderPlayer player)
        |> collage (height*tileSize) (width*tileSize)

main : Signal Element
main = render <~ state