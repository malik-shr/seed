module Statistics exposing (Statistics, calculateStatistics)

import Villager exposing (Villager)

import Utils exposing (tickInYears)

type alias Statistics =
    { femaleCount : Int
    , maleCount : Int
    , childrenCount : Int
    , adultsCount : Int
    , pregnantCount : Int
    , fertileFemaleCount : Int 
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
    in 

    { maleCount = List.length maleCount
    , femaleCount = List.length femaleCount
    , adultsCount = List.length adults
    , childrenCount = List.length children
    , pregnantCount = List.length pregnantCount
    , fertileFemaleCount = List.length fertileWomanCount
    }