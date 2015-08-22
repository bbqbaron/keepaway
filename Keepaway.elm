module Keepaway where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import List exposing (..)
import Maybe exposing (withDefault)
import Signal exposing ((<~), foldp, mailbox, merge)

type Dir = Left|Down|Up|Right

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

update action model = case action of
    _ -> model

state = foldp update init updates.signal

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