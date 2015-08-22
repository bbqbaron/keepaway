module Keepaway where

import Color exposing (..)
import Debug exposing (log)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import List exposing (..)
import Maybe exposing (withDefault)
import Signal exposing ((<~), foldp, mailbox, merge)

type Dir = Left|Down|Up|Right

-- up, right
type alias Point = (Int,Int)

type Action = Idle|Fetch|Move Dir

type Contents = Nothing
type alias Item = ()

type alias Square = Int

type alias Row = List Square
type alias Map = List Row

type alias Model = {
        position: Point,
        carrying: Item,
        grid: Map
    }

updates = mailbox Idle

tileSize = 100
height = 5
width = 5
origin = ((tileSize * height / 2) - (tileSize/2), (tileSize * width / 2 * -1) + (tileSize/2))

init = {
        carrying = (),
        grid = 0 |> repeat width |> repeat height,
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

bound (y,x) = (max 0 (min y (height-1)), max 0 (min x (width-1)))

update action model = case action of
    Move dir -> 
        let (y,x) = model.position
            (y',x') = dirToPoint dir
        in {model|position<-bound (y+y', x+x') |> log "pos"}
    _ -> model

state = foldp update init (merge updates.signal (dirsToAction <~ arrows))

flatten l = foldl (\subl accl -> foldl (::) accl subl) [] l

toPos y x = 
    let (oY, oX) = origin
    in (oY - (toFloat (y*tileSize)), oX + (toFloat (x*tileSize)))

renderPlayer : Point -> Form
renderPlayer (y,x) = filled green (rect tileSize tileSize)
    |> move (toPos y x)

renderSquare : Int -> Int -> Square -> Form
renderSquare y x s = rect tileSize tileSize 
    |> outlined (solid black)
    |> move (toPos y x)

renderRow : Int -> Row -> List Form
renderRow y r = indexedMap (renderSquare y) r

renderMap {grid, position} =
    let h = length grid
        y = map length grid |> maximum |> withDefault 0
    in grid
        |> indexedMap renderRow
        |> flatten
        |> (\l -> (renderPlayer position) :: l)
        |> collage (h*tileSize) (y*tileSize)

main = renderMap <~ state