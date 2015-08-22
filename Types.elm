module Types where

import Dict exposing (Dict)
import Random exposing (Seed)

type Dir = Left|Down|Up|Right

type alias Point = (Int,Int)

type Action = Idle|Fetch|Move Dir|Restart

type alias Item = {name:String, value:Int}
type alias Monster = {name:String, hp:Int}
type alias Class = {name:String}
type alias PC = {name:String, xp:Int, class:Class}

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
        player: Player,
        seed: Seed
    }

emptySquare : Square
emptySquare = {item=Nothing, monster=Nothing, pc=Nothing}