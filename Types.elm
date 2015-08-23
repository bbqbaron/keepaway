module Types where

import Dict exposing (Dict)
import Random exposing (Seed)

type Dir = Left|Down|Up|Right

type alias Point = (Int,Int)

type Action = Idle|Fetch|Move Dir|Restart

type alias Item = {name:String, value:Int}

type alias Monster = {
        cooldown:Int,
        currentCooldown:Int,
        name:String, 
        hp:Int
    }

type Ability = Tough

type alias Class = {
        abilities: List Ability,
        name: String
    }

type StatusType = Stun

type alias Status = {duration:Int, statusType:StatusType}

type alias PC = {
        class:Class,
        name:String,
        statuses:List Status,
        xp:Int
    }

type alias Square = {
        item: Maybe Item,
        monster: Maybe Monster,
        pc: Maybe PC
    }

type alias Player = {
        carrying: Maybe Item,
        position: Point,
        points: Int,
        alive: Bool
    }

type alias Grid = Dict Point Square

type alias Model = {
        grid: Grid,
        player: Player,
        seed: Seed
    }

-- TODO this is used in a few places to get around proper control flow,
-- and not just as a model initializer.
-- Remove these uses.
emptySquare : Square
emptySquare = {item=Nothing, monster=Nothing, pc=Nothing}