module Generators where

import Dict exposing (update)
import List exposing (drop, foldl, head, length)
import Maybe exposing (withDefault)
import Random exposing (customGenerator, generate, Generator, initialSeed, int, pair, Seed)
import Signal exposing ((<~))

import Signal.Time exposing (startTime)

import Const exposing (..)
import Types exposing (..)

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
            class = drop idx classes |> head |> withDefault {name="???"}
        in (class,s')
    )

type alias SpecMaker a = List (Point,a) -> Seed -> (List (Point,a),Seed)
type alias Inserter a = a -> Maybe Square -> Maybe Square

generateItem : SpecMaker Int
generateItem list seed =
    let (p,s') = generate pointGenerator seed
        (val,s'') = generate valueGenerator s'
    in ((p,val)::list,s'')

generateMonster : SpecMaker Int
generateMonster list seed =
    let (p,s') = generate pointGenerator seed
        (hp,s'') = generate hpGenerator s'
    in ((p,hp)::list,s'')

generatePC : SpecMaker Class
generatePC list seed =
    let (p,s') = generate pointGenerator seed
        (class,s'') = generate classGenerator s'
    in ((p,class)::list,s'')

createItemIn : Inserter Int
createItemIn value square =
    case square of
        Just square' -> Just {square'|item<-Just {name="I", value=value}}
        Nothing -> Nothing

addMonster : Inserter Int
addMonster hp s = 
    case s of
        Just s' -> Just {s'|monster<-Just {name="M", hp=hp, cooldown=2, currentCooldown=0}}
        Nothing -> Nothing

addPC : Inserter Class
addPC class s = 
    case s of
        Just s' -> Just {s'|pc<-Just {
            name="PC", 
            class=class, 
            statuses=[],
            xp=0}
        }
        Nothing -> Nothing

addRandom : SpecMaker a -> Inserter a -> Int -> Model -> Model
addRandom genFn addFn howMany model =
    let grid = model.grid
        seed = model.seed
        (specs,s') = foldl (\_ (l,s) -> genFn l s) ([], seed) [0..howMany-1]
        grid' = foldl (\(p,a) g' -> update p (addFn a) g') grid specs
    in {model|
            grid<-grid',
            seed<-s'}

addItems = addRandom generateItem createItemIn numberOfItems
addMonsters = addRandom generateMonster addMonster numberOfMonsters
addPCs = addRandom generatePC addPC 2