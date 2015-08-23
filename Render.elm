module Render where

{-| This does not follow the Elm Architecture. My justification is that the play field is a very large,
complicated single widget, so I'm creating a submodule just to handle rendering. I suppose I could make
each Square a "widget", but the data's awfully intertwined, by necessity.

-}

import Dict exposing (get)
import Graphics.Collage exposing (collage, filled, Form, group, move, outlined, solid, square, text, toForm)
import Graphics.Element exposing (Element, image)
import List exposing (map)
import Maybe exposing (withDefault)
import Text exposing (fromString)

import Html exposing (div, Html)

import Const exposing (..)
import Types exposing (..)
import Util exposing (flatten, origin)

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
                    Nothing -> Maybe.map (\item -> getImage "Gold") item
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