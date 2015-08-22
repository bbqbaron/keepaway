module Keepaway where

import Color exposing (..)
import Debug exposing (log)
import Dict exposing (Dict, empty, get, insert, update)
import Graphics.Collage exposing (..)
import Keyboard exposing (..)
import List exposing (..)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Signal exposing ((<~), dropRepeats, foldp, mailbox, mergeMany)
import Text exposing (fromString)

type Dir = Left|Down|Up|Right

type alias Point = (Int,Int)

type Action = Idle|Fetch|Move Dir

type alias Item = Int
type alias Monster = Int
type alias PC = Int

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

updates = mailbox Idle

tileSize = 75
height = 8
width = 8
origin = (tileSize * (height / 2 - 0.5), tileSize * (width / 2 * -1 + 0.5))

emptySquare = {item=Just 0, monster=Nothing, pc=Nothing}

yRange = [0..height-1]
xRange = [0..width-1]

--items = map (\y -> map (\x -> y*10+x) xRange) yRange

addItem : Maybe Item -> Maybe Square -> Maybe Square
addItem i ms = case ms of
    Just s -> Just {s|item<-i}
    Nothing -> Nothing

addItems : Grid -> Grid
addItems grid =
    foldl (\y g->foldl (\x g'-> update (y,x) (addItem (Just (y*10+x))) g') g xRange) grid yRange

init : Model
init = {
        player={
                carrying = Nothing,
                position = (0,0)
            },
        grid = foldl (\y g -> foldl (\x g'-> insert (y,x) emptySquare g') g [0..width-1]) empty [0..height-1]
            |> addItems
    }

dirsToAction {x,y} = 
    if | x == -1 -> Move Left
       | x == 1 -> Move Right
       | y == -1 -> Move Down
       | y == 1 -> Move Up
       | otherwise -> Idle

dirToPoint d = 
    case d of
        Left -> (1,0)
        Right -> (-1,0)
        Up -> (0,1)
        Down -> (0,-1)
        _ -> (0,0)

bound : (Int,Int) -> (Int,Int)
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

step : Action -> Model -> Model
step action model = case action of
    Move dir -> 
        let player = model.player
            (y,x) = player.position
            (y',x') = dirToPoint dir
            player' = {player|position<-bound (y+y', x+x')}
        in {model|player<-player'}
    Fetch -> swapItems model
    _ -> model

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

flatten l = foldl (\subl accl -> foldl (::) accl subl) [] l

toPos y x = 
    let (oY, oX) = origin
    in (oY - (toFloat (y*tileSize)), oX + (toFloat (x*tileSize)))

renderPlayer : Player -> Form
renderPlayer {carrying, position} = 
    let (y,x) = position
    in filled (if carrying /= Nothing then red else green) (square tileSize)
        |> move (toPos y x)

renderSquare : Int -> Int -> Square -> Form
renderSquare y x {item} = 
    (case item of
        Just n -> n |> toString |> fromString |> text
        Nothing -> outlined (solid red) (square tileSize))
    |> move (toPos y x)

renderRow : Int -> Grid -> List Form
renderRow y g = map (\x->get (y,x) g |> withDefault emptySquare |> renderSquare y x) [0..width-1]

addForm f l = l ++ [f]

renderMap {grid, player} =
    map (\y->renderRow y grid) [0..height-1]
        |> flatten
        |> addForm (renderPlayer player)
        |> collage (height*tileSize) (width*tileSize)

main = renderMap <~ state