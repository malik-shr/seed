module Villager exposing (Villager, viewVillager, breedVillager, giveBirth, ageVillager, moveVillager)

import Svg exposing (svg, circle, rect)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width, x, y)

import Msg exposing (Msg)

import Utils exposing (tickInYears)

type alias Villager =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , age : Int
    , gender : Int 
    , isPregnant : Bool
    , pregnantDuration : Int 
    }

viewVillager : Villager -> Svg.Svg Msg
viewVillager villager =
    let 
        underaged =
            if tickInYears villager.age < 18 then
                True
            else 
                False
    in

    let
        color =
            if villager.gender == 0 then
                if underaged then
                    "pink"
                else 
                    "red"
            else
                if underaged then
                    "lightblue"
                else 
                    "blue"
    in

    circle
        [ cx (String.fromFloat villager.x)
        , cy (String.fromFloat villager.y)
        , r "6"
        , fill (color)
        ]
        []

breedVillager : Villager -> Villager
breedVillager villager =
    let
        canBecomePregnant =
            villager.gender == 0
                && tickInYears villager.age >= 18
                && tickInYears villager.age <= 45
                && not villager.isPregnant

        newIsPregnant =
            villager.isPregnant || canBecomePregnant

        newPregnantDuration =
            if newIsPregnant then
                villager.pregnantDuration + 1

            else
                0
    in
    { villager
        | isPregnant = newIsPregnant
        , pregnantDuration = newPregnantDuration
    }

giveBirth : Int -> Villager -> List Villager
giveBirth tick villager =
    if villager.isPregnant && tickInYears villager.pregnantDuration >= 5 then
        [ { villager
            | isPregnant = False
            , pregnantDuration = 0
          }
        , { x = villager.x
          , y = villager.y
          , vx = 0.1
          , vy = 0.1
          , age = 0
          , gender =
                if modBy 2 tick == 0 then
                    0
                else
                    1
          , isPregnant = False
          , pregnantDuration = 0
          }
        ]

    else
        [ villager ]

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
            if newX < 10 || newX > 390 then
                -villager.vx

            else
                villager.vx

        newVy =
            if newY < 10 || newY > 290 then
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