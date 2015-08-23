module Const where

tileSize = 75
height = 8
width = 8

yRange : List Int
yRange = [0..height-1]

xRange : List Int
xRange = [0..width-1]

numberOfMonsters = 5
numberOfItems = 5
maxMonsterHp = 6
maxItemValue = 6

classes = [
        {name="Cleric"},
        {name="Warlock"},
        {name="Ranger"},
        {name="Shaman"},
        {name="Crusader"},
        {name="Alchemist"},
        {name="Ninja"}
    ]