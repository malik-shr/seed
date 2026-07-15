module Save exposing
    ( SavedGameRow
    , SavedGameState
    , applySavedGameState
    , savedGameRowDecoder
    , savedGameStateDecoder
    , savedGameStateEncoder
    , savedGameStateFromModel
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Model exposing (Model)
import Villager exposing (Villager)


type alias SavedGameState =
    { time : Float
    , villagers : List Villager
    , jobAssignments : List Int
    , nextVillagerId : Int
    , food : Int
    , water : Int
    , money : Float
    , tick : Int
    , pregnancyChances : List Int
    , newVillager : Villager
    , deathCount : Int
    , buildingGrid : List (Maybe Int)
    , draggedBuilding : Maybe Int
    }


type alias SavedGameRow =
    { id : String
    , state : SavedGameState
    }


savedGameStateFromModel : Model -> SavedGameState
savedGameStateFromModel model =
    { time = model.time
    , villagers = model.villagers
    , jobAssignments = model.jobAssignments
    , nextVillagerId = model.nextVillagerId
    , food = model.food
    , water = model.water
    , money = model.money
    , tick = model.tick
    , pregnancyChances = model.pregnancyChances
    , newVillager = model.newVillager
    , deathCount = model.deathCount
    , buildingGrid = model.buildingGrid
    , draggedBuilding = model.draggedBuilding
    }


applySavedGameState : SavedGameState -> Model -> Model
applySavedGameState savedGameState model =
    { model
        | time = savedGameState.time
        , villagers = savedGameState.villagers
        , jobAssignments = savedGameState.jobAssignments
        , nextVillagerId = savedGameState.nextVillagerId
        , food = savedGameState.food
        , water = savedGameState.water
        , money = savedGameState.money
        , tick = savedGameState.tick
        , pregnancyChances = savedGameState.pregnancyChances
        , newVillager = savedGameState.newVillager
        , deathCount = savedGameState.deathCount
        , buildingGrid = savedGameState.buildingGrid
        , draggedBuilding = savedGameState.draggedBuilding
    }


savedGameStateEncoder : SavedGameState -> Encode.Value
savedGameStateEncoder savedGameState =
    Encode.object
        [ ( "time", Encode.float savedGameState.time )
        , ( "villagers", Encode.list villagerEncoder savedGameState.villagers )
        , ( "jobAssignments", Encode.list Encode.int savedGameState.jobAssignments )
        , ( "nextVillagerId", Encode.int savedGameState.nextVillagerId )
        , ( "food", Encode.int savedGameState.food )
        , ( "water", Encode.int savedGameState.water )
        , ( "money", Encode.float savedGameState.money )
        , ( "tick", Encode.int savedGameState.tick )
        , ( "pregnancyChances", Encode.list Encode.int savedGameState.pregnancyChances )
        , ( "newVillager", villagerEncoder savedGameState.newVillager )
        , ( "deathCount", Encode.int savedGameState.deathCount )
        , ( "buildingGrid", Encode.list maybeIntEncoder savedGameState.buildingGrid )
        , ( "draggedBuilding", maybeIntEncoder savedGameState.draggedBuilding )
        ]


savedGameStateDecoder : Decoder SavedGameState
savedGameStateDecoder =
    Decode.map8
        (\time villagers jobAssignments nextVillagerId food water money tick ->
            \pregnancyChances newVillager deathCount buildingGrid draggedBuilding ->
                { time = time
                , villagers = villagers
                , jobAssignments = jobAssignments
                , nextVillagerId = nextVillagerId
                , food = food
                , water = water
                , money = money
                , tick = tick
                , pregnancyChances = pregnancyChances
                , newVillager = newVillager
                , deathCount = deathCount
                , buildingGrid = buildingGrid
                , draggedBuilding = draggedBuilding
                }
        )
        (Decode.field "time" Decode.float)
        (Decode.field "villagers" (Decode.list villagerDecoder))
        (jobAssignmentsDecoder)
        (Decode.field "nextVillagerId" Decode.int)
        (Decode.field "food" Decode.int)
        (Decode.field "water" Decode.int)
        (Decode.field "money" Decode.float)
        (Decode.field "tick" Decode.int)
        |> Decode.andThen
            (\build ->
                Decode.map5
                    (\pregnancyChances newVillager deathCount buildingGrid draggedBuilding ->
                        build pregnancyChances newVillager deathCount buildingGrid draggedBuilding
                    )
                    (Decode.field "pregnancyChances" (Decode.list Decode.int))
                    (Decode.field "newVillager" villagerDecoder)
                    (Decode.field "deathCount" Decode.int)
                    (Decode.field "buildingGrid" (Decode.list maybeIntDecoder))
                    (Decode.field "draggedBuilding" maybeIntDecoder)
            )


savedGameRowDecoder : Decoder SavedGameRow
savedGameRowDecoder =
    Decode.map2 SavedGameRow
        (Decode.field "id" Decode.string)
        (Decode.field "state" savedGameStateDecoder)


villagerEncoder : Villager -> Encode.Value
villagerEncoder villager =
    Encode.object
        [ ( "id", Encode.int villager.id )
        , ( "x", Encode.float villager.x )
        , ( "y", Encode.float villager.y )
        , ( "vx", Encode.float villager.vx )
        , ( "vy", Encode.float villager.vy )
        , ( "age", Encode.int villager.age )
        , ( "food", Encode.int villager.food )
        , ( "water", Encode.int villager.water )
        , ( "gender", Encode.int villager.gender )
        , ( "job", maybeIntEncoder villager.job )
        , ( "isPregnant", Encode.bool villager.isPregnant )
        , ( "pregnantDuration", Encode.int villager.pregnantDuration )
        ]


villagerDecoder : Decoder Villager
villagerDecoder =
    Decode.map8
        (\id x y vx vy age food water ->
            \gender job isPregnant pregnantDuration ->
                { id = id
                , x = x
                , y = y
                , vx = vx
                , vy = vy
                , age = age
                , food = food
                , water = water
                , gender = gender
                , job = job
                , isPregnant = isPregnant
                , pregnantDuration = pregnantDuration
                }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "vx" Decode.float)
        (Decode.field "vy" Decode.float)
        (Decode.field "age" Decode.int)
        (Decode.field "food" Decode.int)
        (Decode.field "water" Decode.int)
        |> Decode.andThen
            (\build ->
                Decode.map4
                    (\gender job isPregnant pregnantDuration ->
                        build gender job isPregnant pregnantDuration
                    )
                    (Decode.field "gender" Decode.int)
                    (Decode.field "job" maybeIntDecoder)
                    (Decode.field "isPregnant" Decode.bool)
                    (Decode.field "pregnantDuration" Decode.int)
            )


maybeIntEncoder : Maybe Int -> Encode.Value
maybeIntEncoder maybeInt =
    case maybeInt of
        Just value ->
            Encode.int value

        Nothing ->
            Encode.null


maybeIntDecoder : Decoder (Maybe Int)
maybeIntDecoder =
    Decode.nullable Decode.int


jobAssignmentsDecoder : Decoder (List Int)
jobAssignmentsDecoder =
    Decode.oneOf
        [ Decode.field "jobAssignments" (Decode.list Decode.int)
        , Decode.field "villagers" (Decode.list villagerDecoder)
            |> Decode.map deriveJobAssignments
        ]


deriveJobAssignments : List Villager -> List Int
deriveJobAssignments villagers =
    List.range 0 7
        |> List.map
            (\rowIndex ->
                villagers
                    |> List.filter (\villager -> villager.job == Just rowIndex)
                    |> List.length
            )
