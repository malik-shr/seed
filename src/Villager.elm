module Villager exposing 
    (Villager
    , viewVillager
    , updatePregnancyDuration
    , ageVillager
    , moveVillager
    , villagerGenerator
    , pregnancyGenerator
    , pregnancyListGenerator
    , deathListGenerator
    , giveBirth
    , useFood
    , useWater
    )

import Svg exposing (Svg, text_)
import Svg.Attributes exposing (x, y, fill, fontSize)
import Svg exposing (text)
import Constants exposing (pregnancyChancePerTick, deathAge)
import Utils exposing (tickInYears)

import Random
import Svg.Attributes exposing (textLength)

type alias Villager =
    { 
    id : Int
    ,x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , age : Int
    , food : Int
    , water : Int
    , gender : Int 
    , job : Maybe Int
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

useFood : Villager -> Villager
useFood villager  =
    {
        villager
        | food = villager.food - 1
    }


useWater : Villager -> Villager
useWater villager =
    { villager
        | water = villager.water - 1
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
            , food = 0
            , water = 0
            , gender = gender
            , job = Nothing
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

deathChance : Int -> Villager -> Float
deathChance lifeExpectancyBonus villager =
    let
        age =
            tickInYears villager.age - lifeExpectancyBonus
    in
    if villager.food < 0 || villager.water < 0 then 
        0.5
    else 
        if age < 50 then
            0.000005
        else if age < 70 then
            0.00005
        else if age < 90 then
            0.0005
        else
            0.005

deathGenerator : Int -> Villager -> Random.Generator (Maybe Villager)
deathGenerator lifeExpectancyBonus villager =
    Random.map
        (\chance ->
            if chance < deathChance lifeExpectancyBonus villager then
                Nothing

            else
                Just villager
        )
        (Random.float 0 1)

deathListGenerator : Int -> List Villager -> Random.Generator (List Villager)
deathListGenerator lifeExpectancyBonus villagers =
    List.foldr
        (\villager acc ->
            Random.map2
                (\maybeVillager list ->
                    case maybeVillager of
                        Just v ->
                            v :: list

                        Nothing ->
                            list
                )
                (deathGenerator lifeExpectancyBonus villager)
                acc
        )
        (Random.constant [])
        villagers



pregnancyGenerator : List Villager -> Villager -> Random.Generator Villager
pregnancyGenerator villagers villager =
   
    let 
        underThousand = List.length villagers < 1000
        hasMale = List.any
            (\v ->
                v.gender == 1
                    && tickInYears v.age >= 18
                    && tickInYears v.age - tickInYears villager.age < 18
                    && tickInYears v.age - tickInYears villager.age > -18
            )
            villagers
    in 

    if
        hasMale
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
    List.foldr
        (\villager acc ->
            Random.map2 (::)
                (pregnancyGenerator villagers villager)
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

        emoji =
            if villager.gender == 0 then
                if villager.isPregnant then "🤰"
                else 
                    if underaged then
                        "👶"
                    else if tickInYears villager.age > 45 then 
                        "👵"
                    else if tickInYears villager.age > 70 then
                        "👩‍🦽‍➡️"
                    else
                        "👩"
            else 
                if underaged then
                    "👶"
                else if tickInYears villager.age > 70 then 
                    "🧑‍🦽‍➡️"
                else
                    "🧔‍♂️"

        size =
            if villager.isPregnant then "26"
            else 
                if underaged then 
                    "18"
                else 
                    "24"
    in
    text_
        [ x (String.fromFloat villager.x )
        , y (String.fromFloat villager.y )
        , fill "black"
        , fontSize size
        ]
        [ text emoji ]
