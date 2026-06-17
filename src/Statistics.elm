module Statistics exposing (Statistics, calculateStatistics)

import Villager exposing (Villager)

import Utils exposing (tickInYears)
import Utils exposing (roundTo2)


type alias Statistics =
    { femaleCount : Int
    , maleCount : Int
    , childrenCount : Int
    , adultsCount : Int
    , pregnantCount : Int
    , fertileFemaleCount : Int 
    , averageAge : Float
    }

calculateStatistics : List Villager -> Statistics
calculateStatistics villagers =
    let 
        maleCount = 
            List.filter
                (\villager ->
                    villager.gender == 1
                )
            villagers

        femaleCount = 
            List.filter
                (\villager ->
                    villager.gender == 0
                )
            villagers
        children = 
            List.filter
                (\villager ->
                    tickInYears villager.age < 18
                )
            villagers
        adults = 
            List.filter
                (\villager ->
                    tickInYears villager.age >= 18
                )
            villagers
        pregnantCount = 
            List.filter
                (\villager ->
                    villager.isPregnant
                )
            villagers
        fertileWomanCount = 
            List.filter
                (\villager ->
                    villager.gender == 0
                    && not villager.isPregnant
                    && tickInYears villager.age >= 18
                    && tickInYears villager.age < 45
                )
            villagers

        averageAge = 
            let
                totalAge =
                    villagers
                        |> List.map (\v -> tickInYears v.age)
                        |> List.sum

                villagerCount =
                    List.length villagers
                average =
                    if villagerCount == 0 then
                        0
                    else
                        toFloat totalAge / toFloat villagerCount
            in
            roundTo2 average
    in 

    { maleCount = List.length maleCount
    , femaleCount = List.length femaleCount
    , adultsCount = List.length adults
    , childrenCount = List.length children
    , pregnantCount = List.length pregnantCount
    , fertileFemaleCount = List.length fertileWomanCount
    , averageAge = averageAge
    }