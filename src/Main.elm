module Main exposing (main)

import Browser
import Browser.Events

import Model exposing (Model)
import Msg exposing (Msg(..))
import View exposing (view)
import Statistics exposing (calculateStatistics)

import Villager exposing 
    (
    updatePregnancyDuration
    , giveBirth
    , ageVillager
    , moveVillager
    , villagerGenerator
    , pregnancyListGenerator
    )

import Random
import Constants exposing (ticksPerYear)
import Villager exposing (deathListGenerator)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , villagers =
            [ { id = 0, x = 80, y = 80, vx = 0.2, vy = 0.1, age = 18 * ticksPerYear, gender = 0, isPregnant = False, pregnantDuration = 0 }
            , { id = 1, x = 150, y = 120, vx = -0.3, vy = 0.5, age = 18 * ticksPerYear, gender = 1, isPregnant = False, pregnantDuration = 0 }
            ]
      , tick = 0
      , nextVillagerId = 0
      , pregnancyChances = []
      , newVillager = { id = 3, x = 80, y = 80, vx = 0.2, vy = 0.1, age = 0, gender = 0, isPregnant = False, pregnantDuration = 0}
      , deathCount = 0
      , statistics = { femaleCount = 0, maleCount = 0, childrenCount = 0, adultsCount = 0, pregnantCount = 0, fertileFemaleCount = 0, averageAge = 0}
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            let
                updatedModel =
                    updateWorld delta model
            in
            ( updatedModel
            , Cmd.batch
                [ Random.generate PregnancyCalculated
                    (pregnancyListGenerator updatedModel.villagers)

                , Random.generate DeathCalculated
                    (deathListGenerator updatedModel.villagers)
                , Random.generate NewVillager
                    (villagerGenerator updatedModel.nextVillagerId)
                ]
            )

        DeathCalculated updatedVillagers ->
            let
                diedThisTick =
                    List.length model.villagers - List.length updatedVillagers
            in
            ( { model
                | villagers = updatedVillagers
                , deathCount = model.deathCount + diedThisTick
                , statistics = calculateStatistics updatedVillagers
            }
            , Cmd.none
            )

        PregnancyCalculated updatedVillagers ->
            ( { model | villagers = updatedVillagers }
            , Cmd.none
            )

        GenNewVillagerValues ->
            ( model
            , Random.generate NewVillager
                (villagerGenerator model.nextVillagerId)
            )

        NewVillager newVillager ->
            ( { model
                | newVillager = newVillager
            }
            , Cmd.none
            )

updateWorld : Float -> Model -> Model
updateWorld delta model =
    let
        updatedVillagers =
            model.villagers
                |> List.map moveVillager
                |> List.map ageVillager
                |> List.map updatePregnancyDuration
                |> List.concatMap (giveBirth model.newVillager)
    in
    { model
        | time = model.time + delta
        , tick = model.tick + 1
        , villagers = updatedVillagers
        , statistics = calculateStatistics updatedVillagers
    }





