module Types where

import Dict exposing (Dict)

type Dir = Left|Down|Up|Right

type alias Point = (Int,Int)

type Action = Idle|Fetch|Move Dir|Restart

type alias Item = {name:String, value:Int}
type alias Monster = {name:String, hp:Int}
type alias PC = {name:String, xp:Int}

type alias Square = {
        item: Maybe Item,
        monster: Maybe Monster,
        pc: Maybe PC
    }

type alias Player = {
        carrying: Maybe Item,
        position: Point,
        points: Int,
        alive:Bool
    }

type alias Grid = Dict Point Square

type alias Model = {
        grid: Grid,
        player: Player
    }

emptySquare : Square
emptySquare = {item=Just {name="I", value=0}, monster=Nothing, pc=Nothing}