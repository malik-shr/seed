module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav

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
    , useFood
    , Villager
    )

import Random
import Constants exposing (gridCellCount, gridColumns, gridRows, ticksPerYear)
import Villager exposing (deathListGenerator)
import Url exposing (Url)
import List exposing (length)

type alias Flags =
    { tileImage : String
    , buildingImages : List String
    }

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = applicationView
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


applicationView : Model -> Browser.Document Msg
applicationView model =
    { title = "Seed"
    , body = [ view model ]
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { time = 0
      , villagers =
            [ { id = 0, x = 80, y = 80, vx = 0.2, vy = 0.1, food = 0, age = 18 * ticksPerYear, gender = 0, isPregnant = False, pregnantDuration = 0 }
            , { id = 1, x = 150, y = 120, vx = -0.3, vy = 0.5, food = 0, age = 18 * ticksPerYear, gender = 1, isPregnant = False, pregnantDuration = 0 }
            ]
      , tick = 0
      , nextVillagerId = 0
      , food = 0
      , pregnancyChances = []
      , newVillager = { id = 3, x = 80, y = 80, vx = 0.2, vy = 0.1, food = 0, age = 0, gender = 0, isPregnant = False, pregnantDuration = 0}
      , deathCount = 0
      , statistics = { femaleCount = 0, maleCount = 0, childrenCount = 0, adultsCount = 0, pregnantCount = 0, fertileFemaleCount = 0, averageAge = 0}
      , filledGridRows = List.repeat gridRows 0
      , buildingGrid = List.repeat gridCellCount Nothing
      , draggedBuilding = Nothing
      , tileImage = flags.tileImage
      , buildingImages = flags.buildingImages
      , key = key
      , url = url
      , worldCalculationPending = False
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

                worldCommand =
                    if model.worldCalculationPending then
                        Cmd.none

                    else
                        Random.generate WorldCalculated
                            (worldGenerator updatedModel.villagers)

                nextModel =
                    if model.worldCalculationPending then
                        updatedModel

                    else
                        { updatedModel | worldCalculationPending = True }
            in
            ( nextModel
            , Cmd.batch
                [ worldCommand
                , Random.generate NewVillager
                    (villagerGenerator updatedModel.nextVillagerId)
                ]
            )

        FeedVillagers ->
            let
                amountToFeed =
                    min model.food (List.length model.villagers)

                updatedVillagers =
                    List.indexedMap
                        (\index villager ->
                            if index < amountToFeed then
                                { villager | food = villager.food + 1 }

                            else
                                villager
                        )
                        model.villagers

                updatedModel =
                    { model
                        | food = model.food - amountToFeed
                        , villagers = updatedVillagers
                    }
            in
            ( updatedModel, Cmd.none )


        WorldCalculated updatedVillagers ->
            let
                diedThisTick =
                    max 0 (List.length model.villagers - List.length updatedVillagers)
            in
            ( { model
                | villagers = updatedVillagers
                , deathCount = model.deathCount + diedThisTick
                , statistics = calculateStatistics updatedVillagers
                , worldCalculationPending = False
            }
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

        FillGridRow rowIndex ->
            ( fillNextCellInRow rowIndex model, Cmd.none )

        StartDraggingBuilding buildingIndex ->
            ( { model | draggedBuilding = Just buildingIndex }, Cmd.none )

        DropBuilding cellIndex ->
            case model.draggedBuilding of
                Just buildingIndex ->
                    ( placeBuilding cellIndex buildingIndex model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        StopDraggingBuilding ->
            ( { model | draggedBuilding = Nothing }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )


worldGenerator : List Villager -> Random.Generator (List Villager)
worldGenerator villagers =
    pregnancyListGenerator villagers
        |> Random.andThen deathListGenerator


fillGridRow : Int -> List Int -> List Int
fillGridRow targetRow filledGridRows =
    filledGridRows
        |> List.indexedMap
            (\rowIndex filledCells ->
                if rowIndex == targetRow then
                    min gridColumns (filledCells + 1)

                else
                    filledCells
            )


fillNextCellInRow : Int -> Model -> Model
fillNextCellInRow rowIndex model =
    let
        filledCells =
            model.filledGridRows
                |> List.drop rowIndex
                |> List.head
                |> Maybe.withDefault 0

        cellIndex =
            rowIndex * gridColumns + filledCells
    in
    if filledCells >= gridColumns then
        model

    else
        { model
            | filledGridRows = fillGridRow rowIndex model.filledGridRows
            , buildingGrid = setAt cellIndex (Just rowIndex) model.buildingGrid
        }


placeBuilding : Int -> Int -> Model -> Model
placeBuilding cellIndex buildingIndex model =
    { model
        | buildingGrid = setAt cellIndex (Just buildingIndex) model.buildingGrid
        , filledGridRows = filledRowsFromGrid (setAt cellIndex (Just buildingIndex) model.buildingGrid)
        , draggedBuilding = Nothing
    }


setAt : Int -> a -> List a -> List a
setAt targetIndex value values =
    values
        |> List.indexedMap
            (\index currentValue ->
                if index == targetIndex then
                    value

                else
                    currentValue
            )


filledRowsFromGrid : List (Maybe Int) -> List Int
filledRowsFromGrid buildingGrid =
    List.range 0 (gridRows - 1)
        |> List.map
            (\rowIndex ->
                buildingGrid
                    |> List.drop (rowIndex * gridColumns)
                    |> List.take gridColumns
                    |> List.filterMap identity
                    |> List.length
            )

updateWorld : Float -> Model -> Model
updateWorld delta model =
    let
        processedVillagers =
            model.villagers
                |> List.map moveVillager
                |> List.map ageVillager
                |> List.map useFood
                |> List.map updatePregnancyDuration
                |> List.concatMap (giveBirth model.newVillager)

        producedFood =
            calculateFood model

        ( remainingFood, fedVillagers ) =
            feedOneRound producedFood processedVillagers
    in
    { model
        | time = model.time + delta
        , food = remainingFood
        , tick = model.tick + 1
        , villagers = fedVillagers
        , statistics = calculateStatistics fedVillagers
    }

feedOneRound : Int -> List Villager -> ( Int, List Villager )
feedOneRound availableFood villagers =
    let
        amountToFeed =
            min availableFood (List.length villagers)

        updatedVillagers =
            List.indexedMap
                (\index villager ->
                    if index < amountToFeed then
                        { villager | food = villager.food + 1 }

                    else
                        villager
                )
                villagers
    in
    ( availableFood - amountToFeed, updatedVillagers )

calculateFood: Model -> Int 
calculateFood model =
    let
        newFood = 
                (model.food + 15) 
    in
    
    newFood




