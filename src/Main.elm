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
    , giveBirth, ageVillager
    , moveVillager
    , villagerGenerator
    , removeDeadVillagers
    , pregnancyListGenerator
    )

import Random

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
            [ { id = 0, x = 80, y = 80, vx = 0.2, vy = 0.1, age = 0, gender = 0, isPregnant = False, pregnantDuration = 0 }
            , { id = 1, x = 150, y = 120, vx = -0.3, vy = 0.5, age = 0, gender = 1, isPregnant = False, pregnantDuration = 0 }
            ]
      , tick = 0
      , nextVillagerId = 0
      , pregnancyChances = []
      , newVillager = { id = 3, x = 80, y = 80, vx = 0.2, vy = 0.1, age = 0, gender = 0, isPregnant = False, pregnantDuration = 0}
      , deathCount = 0
      , statistics = { femaleCount = 0, maleCount = 0, childrenCount = 0, adultsCount = 0, pregnantCount = 0, fertileFemaleCount = 0}
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

                , Random.generate NewVillager
                    (villagerGenerator updatedModel.nextVillagerId)
                ]
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
        beforeDeathCheck =
            model.villagers
                |> List.map moveVillager
                |> List.map ageVillager
                |> List.map updatePregnancyDuration
                |> List.concatMap (giveBirth model.newVillager)

        updatedVillagers =
            removeDeadVillagers beforeDeathCheck

        diedThisTick =
            List.length beforeDeathCheck - List.length updatedVillagers
    in
    { model
        | time = model.time + delta
        , tick = model.tick + 1
        , deathCount = model.deathCount + diedThisTick
        , villagers = updatedVillagers
        , statistics = calculateStatistics updatedVillagers
    }






