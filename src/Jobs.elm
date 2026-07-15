module Jobs exposing
    ( JobEffect
    , allJobRows
    , assignedWorkerCount
    , jobCapacity
    , jobEffectForRow
    , jobEffectSummary
    , jobName
    )

import List exposing (length)
import Villager exposing (Villager)


type alias JobEffect =
    { food : Int
    , water : Int
    , money : Int
    , lifeExpectancy : Int
    }


allJobRows : List Int
allJobRows =
    List.range 0 7


jobName : Int -> String
jobName rowIndex =
    case rowIndex of
        0 ->
            "Häuser"

        1 ->
            "Farmen"

        2 ->
            "Schulen"

        3 ->
            "Kaufhäuser"

        4 ->
            "Tavernen"

        5 ->
            "Brunnen"

        6 ->
            "Getreidespeicher"

        7 ->
            "Bäckereien"

        _ ->
            "Job " ++ String.fromInt (rowIndex + 1)


jobEffectForRow : Int -> JobEffect
jobEffectForRow rowIndex =
    case rowIndex of
        0 ->
            { food = 0, water = 0, money = 0, lifeExpectancy = 1 }

        1 ->
            { food = 1, water = 0, money = 0, lifeExpectancy = 0 }

        2 ->
            { food = 0, water = 0, money = 0, lifeExpectancy = 1 }

        3 ->
            { food = 0, water = 0, money = 1, lifeExpectancy = 0 }

        4 ->
            { food = 0, water = 0, money = 1, lifeExpectancy = 0 }

        5 ->
            { food = 0, water = 1, money = 0, lifeExpectancy = 0 }

        6 ->
            { food = 1, water = 0, money = 0, lifeExpectancy = 0 }

        7 ->
            { food = 1, water = 0, money = 0, lifeExpectancy = 0 }

        _ ->
            { food = 0, water = 0, money = 0, lifeExpectancy = 0 }


jobEffectSummary : Int -> String
jobEffectSummary rowIndex =
    let
        effect =
            jobEffectForRow rowIndex

        parts =
            [ if effect.food > 0 then
                Just ("Essen +" ++ String.fromInt effect.food)

              else
                Nothing
            , if effect.water > 0 then
                Just ("Wasser +" ++ String.fromInt effect.water)

              else
                Nothing
            , if effect.money > 0 then
                Just ("Geld +" ++ String.fromInt effect.money)

              else
                Nothing
            , if effect.lifeExpectancy > 0 then
                Just ("Lebenserwartung +" ++ String.fromInt effect.lifeExpectancy)

              else
                Nothing
            ]
                |> List.filterMap identity
    in
    case parts of
        [] ->
            "Kein direkter Bonus"

        _ ->
            String.join ", " parts


jobCapacity : Int -> List Int -> Int
jobCapacity rowIndex filledGridRows =
    filledGridRows
        |> List.drop rowIndex
        |> List.head
        |> Maybe.withDefault 0


assignedWorkerCount : Int -> List Villager -> Int
assignedWorkerCount rowIndex villagers =
    villagers
        |> List.filter (\villager -> villager.job == Just rowIndex)
        |> length
