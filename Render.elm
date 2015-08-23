module Render where

{-| This does not follow the Elm Architecture. My justification is that the play field is a very large,
complicated single widget, so I'm creating a submodule just to handle rendering. I suppose I could make
each Square a "widget", but the data's awfully intertwined, by necessity.

-}

import Dict exposing (get)
import Graphics.Collage exposing (collage, filled, Form, group, move, moveX, outlined, solid, square, text, toForm)
import Graphics.Element exposing (Element, image)
import List exposing (filter, head, map, reverse, sortBy)
import Maybe exposing (andThen, oneOf, withDefault)
import Text exposing (fromString)

import Html exposing (div, Html, p)

import Const exposing (..)
import Types exposing (..)
import Util exposing (cond, flatten, origin, isStun)

toPos : Int -> Int -> (Float, Float)
toPos y x = 
    let (oY, oX) = origin
    in (oY - (toFloat (y*tileSize)), oX + (toFloat (x*tileSize)))

renderPlayer : Player -> Form
renderPlayer {carrying, position} = 
    let (y,x) = position
        base = getImage "Imp"
        withText = 
            carrying
            |> Maybe.map (\i -> group [base, i |> toString |> fromString |> text])
            |> withDefault base
        in withText |> move (toPos y x)

getImage : String -> Form
getImage name = "assets/art/"++name++".png"
    |> image tileSize tileSize
    |> toForm

renderPC : PC -> Form
renderPC pc =
    let base = getImage pc.class.name
        renderStun stun = 
            stun.duration
                |> toString
                |> fromString
                |> text
    in filter isStun pc.statuses
            |> sortBy (.duration)
            |> reverse
            |> head
            |> Maybe.map renderStun
            |> Maybe.map (flip (::) [base]>>reverse>>group)
            |> withDefault base

renderMonster : Monster -> Form
renderMonster monster =
    let base = [getImage "Goblin", monster.hp |> toString |> fromString |> text |> moveX -20]
        cooldown = monster.currentCooldown
    in cond
        (cooldown > 0)
        (base ++ [cooldown |> toString |> fromString |> text])
        base
        |> group

renderItem : Item -> Form
renderItem {value} =group [getImage "Gold", value |> toString |> fromString |> text]

renderSquare : Int -> Int -> Square -> Form
renderSquare y x {item, monster, pcs} = 
    let ground = getImage "Ground"
        grp = oneOf [
                (head pcs |> Maybe.map renderPC),
                (monster |> Maybe.map renderMonster),
                (item |> Maybe.map renderItem)
            ]
            |> Maybe.map ((flip (::) [ground])>>reverse>>group)
            |> withDefault ground
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
renderPoints model = "Points: " ++ toString model.player.points 
    |> Html.text

renderRules : Html
renderRules =
    [
        "Welcome to Imp.",
        "You are an imp. Your arbitrary square of ground is under attack by intrepid busybody PCs
            in search of phat lewtz. You cannot defeat them, but you can delay and frustrate them for as long
            as possible. Put the gold where it's hardest to reach.",
        "You can pick up/drop a stack of gold with Space.",
        "Move with the arrows. Each movement triggers a Turn.",
        "Each Turn, PCs will:",
        "   * if in a square with a Goblin, reduce its hit points by 1",
        "   * otherwise, move one square toward gold",
        "Each Turn, Goblins will stun a nearby PC for a certain number of Turns. This ability has a cooldown.",
        "Each Turn, for each stack of gold on the field, you earn points equal to its value. Keep it going!",
        "The game ends when no gold remains on the field.",
        "Press R to begin."
    ]
    |> map (Html.text>>flip (::) []>>p [])
    |> div []

render : Model -> Html
render model =
    if model.player.alive then
        div [] [
            renderGrid model |> Html.fromElement,
            renderPoints model
        ]
    else
        renderRules