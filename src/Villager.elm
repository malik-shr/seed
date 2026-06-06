module Villager exposing 
    (Villager
    , viewVillager
    , updatePregnancyDuration
    , giveBirth
    , ageVillager
    , moveVillager
    , villagerGenerator
    , pregnancyGenerator
    , pregnancyListGenerator
    , removeDeadVillagers
    )

import Svg exposing (circle)
import Svg.Attributes exposing (cx, cy, fill, r, x, y)
import Constants exposing (pregnancyChancePerTick, deathAge)
import Utils exposing (tickInYears)

import Random

type alias Villager =
    { 
    id : Int
    ,x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , age : Int
    , gender : Int 
    , isPregnant : Bool
    , pregnantDuration : Int 
    }

updatePregnancyDuration : Villager -> Villager
updatePregnancyDuration villager =
    if villager.isPregnant then
        { villager | pregnantDuration = villager.pregnantDuration + 1 }

    else
        villager

giveBirth : Villager -> Villager -> List Villager
giveBirth child mother =
    if mother.isPregnant && tickInYears mother.pregnantDuration >= 1 then
        [ { mother
            | isPregnant = False
            , pregnantDuration = 0
          }
        , child
        ]

    else
        [ mother ]

ageVillager : Villager -> Villager 
ageVillager villager = 
    let 
        newAge = 
            villager.age + 1
    in

    { villager
        | age = newAge
    }

moveVillager : Villager -> Villager
moveVillager villager =
    let
        newX =
            villager.x + villager.vx

        newY =
            villager.y + villager.vy

        newVx =
            if newX < 10 || newX > 790 then
                -villager.vx

            else
                villager.vx

        newVy =
            if newY < 10 || newY > 590 then
                -villager.vy

            else
                villager.vy
    in
    { villager
        | x = newX
        , y = newY
        , vx = newVx
        , vy = newVy
    }

villagerGenerator : Int -> Random.Generator Villager
villagerGenerator id =
    Random.map5
        (\x y vx vy gender ->
            { id = id
            , x = x
            , y = y
            , vx = vx
            , vy = vy
            , age = 0
            , gender = gender
            , isPregnant = False
            , pregnantDuration = 0
            }
        )
        (Random.float 10 750)
        (Random.float 10 550)
        (randomVelocity)
        (randomVelocity)
        (Random.int 0 1)

randomVelocity : Random.Generator Float
randomVelocity =
    Random.map2
        (\speed sign ->
            if sign == 0 then
                speed
            else
                -speed
        )
        (Random.float 0.5 1)
        (Random.int 0 1)

pregnancyGenerator : Bool ->  Bool -> Villager -> Random.Generator Villager
pregnancyGenerator hasMale underThousand villager =
    if
        hasMale
            && underThousand
            && villager.gender == 0
            && tickInYears villager.age >= 18
            && tickInYears villager.age <= 45
            && not villager.isPregnant
    then
        Random.map
            (\chance ->
                if chance < pregnancyChancePerTick then
                    { villager
                        | isPregnant = True
                        , pregnantDuration = 0
                    }

                else
                    villager
            )
            (Random.float 0 1)

    else
        Random.constant villager



pregnancyListGenerator : List Villager -> Random.Generator (List Villager)
pregnancyListGenerator villagers =
    let 
        underThousand = List.length villagers < 1000
        hasMale = List.any
            (\villager ->
                villager.gender == 1
                    && tickInYears villager.age >= 18
            )
            villagers
    in 

    List.foldr
        (\villager acc ->
            Random.map2 (::)
                (pregnancyGenerator hasMale underThousand villager)
                acc
        )
        (Random.constant [])
        villagers

removeDeadVillagers : List Villager -> List Villager
removeDeadVillagers villagers =
    List.filter
        (\villager ->
            tickInYears villager.age < deathAge
        )
        villagers

viewVillager : Villager -> Svg.Svg msg
viewVillager villager =
    let
        underaged =
            tickInYears villager.age < 18

        color =
            if villager.gender == 0 then
                if villager.isPregnant then "purple"
                else 
                    if underaged then
                        "pink"
                    else
                        "red"
            else if underaged then
                "lightblue"
            else
                "blue"

        size =
            if villager.isPregnant then "8"
            else 
                if underaged then 
                    "3"
                else 
                    "6"
    in
    circle
        [ cx (String.fromFloat villager.x)
        , cy (String.fromFloat villager.y)
        , r size
        , fill color
        ]
        []