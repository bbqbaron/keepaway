module Keepaway where

import Color exposing (..)
import Dict exposing (Dict, empty, get, insert)
import Graphics.Collage exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import List exposing (..)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Signal exposing ((<~), foldp, mailbox, mergeMany)

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

type alias Grid = Dict Point Square

type alias Model = {
        position: Point,
        carrying: Maybe Item,
        grid: Grid
    }

updates = mailbox Idle

tileSize = 75
height = 8
width = 8
origin = (tileSize * (height / 2 - 0.5), tileSize * (width / 2 * -1 + 0.5))

zip2 : List a -> List a -> List (List a)
zip2 = map2 (\i1 i2 -> [i1,i2])

init : Model
init = {
        carrying = Nothing,
        grid = 
            let emptySquare = {item=Nothing, monster=Nothing, pc=Nothing}
            in foldl (\[x,y] g -> insert (x,y) emptySquare g) empty (zip2 [0..height-1] [0..width-1]),
        position = (2,2)
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

(~|>) : Maybe a -> (a -> Maybe b) -> Maybe b
(~|>) = andThen

update : Action -> Model -> Model
update action model = case action of
    Move dir -> 
        let (y,x) = model.position
            (y',x') = dirToPoint dir
        in {model|position<-bound (y+y', x+x')}
    Fetch ->
        get (model.position) model.grid
            ~|> (((.item) >> (\i -> {model|carrying<-i})) >> Just)
            |> withDefault model
    _ -> model

state = foldp update init 
    (mergeMany 
        [
            updates.signal,
            (dirsToAction <~ arrows),
            space 
                |> foldp (\n o->o && not n) False 
                |> Signal.map (\b->if b then Fetch else Idle) 
                |> Signal.dropRepeats
        ]
    )

flatten l = foldl (\subl accl -> foldl (::) accl subl) [] l

toPos y x = 
    let (oY, oX) = origin
    in (oY - (toFloat (y*tileSize)), oX + (toFloat (x*tileSize)))

renderItem (y,x) = filled red (rect tileSize tileSize)
    |> move (toPos y x)

renderPlayer : Point -> Form
renderPlayer (y,x) = filled green (rect tileSize tileSize)
    |> move (toPos y x)

renderSquare : Int -> Int -> Maybe Square -> Form
renderSquare y x s = rect tileSize tileSize 
    |> outlined (solid black)
    |> move (toPos y x)

renderRow : Int -> Grid -> List Form
renderRow y g = map (\x->get (y,x) g |> renderSquare y x) [0..width-1]

addForm f l = f :: l

renderMap {grid, position} =
    map (\y->renderRow y grid) [0..height-1]
        |> flatten
        |> addForm (renderPlayer position)
        |> addForm (renderItem (1,1))
        |> collage (height*tileSize) (width*tileSize)

main = renderMap <~ state