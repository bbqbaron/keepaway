module Generators where

import Dict exposing (update)
import List exposing (drop, foldl, head, length)
import Maybe exposing (withDefault)
import Random exposing (customGenerator, generate, Generator, initialSeed, int, pair, Seed)
import Signal exposing ((<~))

import Signal.Time exposing (startTime)

import Const exposing (classes, height, maxItemValue, maxMonsterHp, numberOfItems, numberOfMonsters, numberOfPCs, width)
import Types exposing (Class, Model, Point, Square)

-- seed

startTimeSeed : Signal Seed
startTimeSeed = initialSeed << round <~ startTime

-- generators

pointGenerator : Generator (Int,Int)
pointGenerator = pair (int 0 (height-1)) (int 0 (width-1))

hpGenerator = int 1 maxMonsterHp
valueGenerator = int 1 maxItemValue

classNumGenerator = int 0 ((length classes)-1)

classGenerator = customGenerator (\seed ->
        let (idx,s') = generate classNumGenerator seed
            class = drop idx classes |> head |> withDefault {abilities=[], name="???"}
        in (class,s')
    )

type alias SpecMaker a = List (Point,a) -> Seed -> (List (Point,a),Seed)
type alias Inserter a = a -> Maybe Square -> Maybe Square

generateWith : Generator a -> SpecMaker a
generateWith gen list seed =
    let (p,s') = generate pointGenerator seed
        (v,s'') = generate gen s'
    in  ((p,v)::list,s'')

generateItem : SpecMaker Int
generateItem = generateWith valueGenerator

generateMonster : SpecMaker Int
generateMonster = generateWith hpGenerator

generatePC : SpecMaker Class
generatePC = generateWith classGenerator

inserter : (a -> Square -> Square) -> Inserter a
inserter inserter value square = Maybe.map (\s->inserter value s) square

addItem : Inserter Int
addItem = inserter (\value square -> {square|item<-Just {name="I", value=value}})

addMonster : Inserter Int
addMonster = inserter (\hp square -> {square|monster<-Just {name="M", hp=hp, cooldown=4, currentCooldown=0}})

addPC : Inserter Class
addPC = inserter (\class square -> {square|pc<-Just {
            name="PC",
            class=class,
            statuses=[],
            xp=0}
        })

addRandom : SpecMaker a -> Inserter a -> Int -> Model -> Model
addRandom genFn addFn howMany model =
    let grid = model.grid
        seed = model.seed
        (specs,s') = foldl (\_ (l,s) -> genFn l s) ([], seed) [0..howMany-1]
        grid' = foldl (\(p,a) g' -> update p (addFn a) g') grid specs
    in {model|
            grid<-grid',
            seed<-s'}

addItems = addRandom generateItem addItem numberOfItems
addMonsters = addRandom generateMonster addMonster numberOfMonsters
addPCs = addRandom generatePC addPC numberOfPCs