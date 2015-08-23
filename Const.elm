module Const where

import Types exposing (Ability(..))

tileSize = 75
height = 8
width = 8

yRange : List Int
yRange = [0..height-1]

xRange : List Int
xRange = [0..width-1]

numberOfItems = 5
numberOfMonsters = 5
numberOfPCs = 2

maxItemValue = 6
maxMonsterHp = 6

classes = [
        {abilities=[], name="Cleric"},
        {abilities=[], name="Warlock"},
        {abilities=[], name="Ranger"},
        {abilities=[], name="Shaman"},
        {abilities=[Tough], name="Crusader"},
        {abilities=[], name="Alchemist"},
        {abilities=[], name="Ninja"}
    ]