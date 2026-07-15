port module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
import String

import Jobs exposing (allJobRows, jobCapacity, jobEffectForRow)
import Model exposing (Model)
import Msg exposing (LoginResult, Msg(..))
import Route exposing (fromUrl, shareUrl)
import Save exposing (applySavedGameState, savedGameRowDecoder, savedGameStateEncoder, savedGameStateFromModel)
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
    , useWater
    , Villager
    )

import Random
import Constants exposing (gridCellCount, gridColumns, gridRows, ticksPerYear)
import Villager exposing (deathListGenerator)
import Url exposing (Url)

port requestPostgrestToken : { username : String, password : String } -> Cmd msg

port postgrestTokenReceived : (Decode.Value -> msg) -> Sub msg

type alias Flags =
    { tileImage : String
    , buildingImages : List String
    , appBaseUrl : String
    , postgrestUrl : String
    , postgrestSchema : String
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
    let
        routeInfo =
            fromUrl url

        baseModel =
            { time = 0
            , villagers =
                [ { id = 0, x = 80, y = 80, vx = 0.2, vy = 0.1, food = 0, water = 0, age = 18 * ticksPerYear, gender = 0, job = Nothing, isPregnant = False, pregnantDuration = 0 }
                , { id = 1, x = 150, y = 120, vx = -0.3, vy = 0.5, food = 0, water = 0, age = 18 * ticksPerYear, gender = 1, job = Nothing, isPregnant = False, pregnantDuration = 0 }
                ]
            , tick = 0
            , nextVillagerId = 0
            , food = 0
            , foodPerTick = 0
            , pregnancyChances = []
            , newVillager = { id = 3, x = 80, y = 80, vx = 0.2, vy = 0.1, food = 0, water = 0, age = 0, gender = 0, job = Nothing, isPregnant = False, pregnantDuration = 0 }
            , deathCount = 0
            , statistics = { femaleCount = 0, maleCount = 0, childrenCount = 0, adultsCount = 0, pregnantCount = 0, fertileFemaleCount = 0, averageAge = 0 }
            , filledGridRows = List.repeat gridRows 0
            , buildingGrid = List.repeat gridCellCount Nothing
            , draggedBuilding = Nothing
            , tileImage = flags.tileImage
            , buildingImages = flags.buildingImages
            , appBaseUrl = flags.appBaseUrl
            , postgrestUrl = flags.postgrestUrl
            , postgrestSchema = flags.postgrestSchema
            , postgrestToken = ""
            , postgrestUsername = "www26_apesf_aquhs"
            , postgrestPassword = ""
            , authInProgress = False
            , authMessage =
                case routeInfo.saveId of
                    Just _ ->
                        Just "Bitte anmelden, um den Spielstand zu laden"

                    Nothing ->
                        Nothing
            , saveId = routeInfo.saveId
            , savePersisted = False
            , saving = False
            , loadingSave =
                case routeInfo.saveId of
                    Just _ ->
                        False

                    Nothing ->
                        False
            , persistenceMessage = Nothing
            , key = key
            , url = url
            , worldCalculationPending = False
            , water = 0
            , waterPerTick = 0
            , money = 100
            , moneyPerTick = 0
            }
    in
    ( baseModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , postgrestTokenReceived PostgrestTokenReceived
        ]


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
                            (worldGenerator updatedModel)

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

        ToggleJobAssignment villagerId jobIndex ->
            ( toggleJobAssignment villagerId jobIndex model, Cmd.none )

        LoginUsernameChanged username ->
            ( { model | postgrestUsername = username, authMessage = Nothing }, Cmd.none )

        LoginPasswordChanged password ->
            ( { model | postgrestPassword = password, authMessage = Nothing }, Cmd.none )

        LoginRequested ->
            if String.isEmpty model.postgrestUrl then
                ( { model | authMessage = Just "Keine PostgREST-URL konfiguriert" }, Cmd.none )

            else if String.isEmpty model.postgrestUsername || String.isEmpty model.postgrestPassword then
                ( { model | authMessage = Just "Bitte Nutzername und Passwort eingeben" }, Cmd.none )

            else if model.authInProgress then
                ( model, Cmd.none )

            else
                ( { model
                    | authInProgress = True
                    , authMessage = Just "Hole Login-Token..."
                  }
                , requestPostgrestToken
                    { username = model.postgrestUsername
                    , password = model.postgrestPassword
                    }
                )

        PostgrestTokenReceived rawValue ->
            case Decode.decodeValue postgrestTokenDecoder rawValue of
                Ok loginResult ->
                    if loginResult.ok then
                        let
                            authenticatedModel =
                                { model
                                    | authInProgress = False
                                    , postgrestToken = loginResult.token
                                    , postgrestPassword = ""
                                    , authMessage = Just "Angemeldet"
                                }
                        in
                        case model.saveId of
                            Just saveId ->
                                ( { authenticatedModel | loadingSave = True }
                                , loadSaveCommand authenticatedModel saveId
                                )

                            Nothing ->
                                ( authenticatedModel, Cmd.none )

                    else
                        ( { model
                            | authInProgress = False
                            , authMessage = Just loginResult.error
                          }
                        , Cmd.none
                        )

                Err decodeError ->
                    ( { model
                        | authInProgress = False
                        , authMessage = Just ("Ungültige Login-Antwort: " ++ Decode.errorToString decodeError)
                      }
                    , Cmd.none
                    )

        SaveRequested ->
            if not (canUsePostgrestModel model) then
                ( { model | authMessage = Just "Bitte zuerst anmelden" }, Cmd.none )

            else if model.saving || model.loadingSave then
                ( model, Cmd.none )

            else if model.saveId == Nothing then
                ( { model
                    | saving = True
                    , persistenceMessage = Just "Erzeuge Spielstand-ID..."
                  }
                , Random.generate SaveIdGenerated generateSaveId
                )

            else
                ( { model
                    | saving = True
                    , persistenceMessage = Just "Speichere Spielstand..."
                  }
                , saveGameCommand model
                )

        SaveIdGenerated saveId ->
            let
                updatedModel =
                    { model
                        | saveId = Just saveId
                        , saving = True
                        , persistenceMessage = Just "Speichere Spielstand..."
                    }
            in
            ( updatedModel, saveGameCommand updatedModel )

        SaveCompleted saveResult ->
            case saveResult of
                Ok () ->
                    let
                        currentTab =
                            (fromUrl model.url).tab

                        savePath =
                            shareUrl model.appBaseUrl model.saveId currentTab
                    in
                    case model.saveId of
                        Just saveId ->
                            ( { model
                                | saveId = Just saveId
                                , savePersisted = True
                                , saving = False
                                , persistenceMessage = Just "Spielstand gespeichert"
                              }
                            , Nav.replaceUrl model.key savePath
                            )

                        Nothing ->
                            ( { model
                                | saving = False
                                , persistenceMessage = Just "Speichern fehlgeschlagen"
                              }
                            , Cmd.none
                            )

                Err errorMessage ->
                    ( { model
                        | saving = False
                        , persistenceMessage = Just errorMessage
                      }
                    , Cmd.none
                    )

        SaveLoaded loadResult ->
            case loadResult of
                Ok savedRows ->
                    case savedRows of
                        savedRow :: _ ->
                            let
                                loadedModel =
                                    model
                                        |> applySavedGameState savedRow.state
                                        |> refreshDerivedFields
                            in
                            ( { loadedModel
                                | saveId = Just savedRow.id
                                , savePersisted = True
                                , loadingSave = False
                                , persistenceMessage = Just "Spielstand geladen"
                              }
                            , Cmd.none
                            )

                        [] ->
                            ( { model
                                | loadingSave = False
                                , saveId = Nothing
                                , persistenceMessage = Just "Kein Spielstand gefunden"
                              }
                            , Cmd.none
                            )

                Err errorMessage ->
                    ( { model
                        | loadingSave = False
                        , saveId = Nothing
                        , persistenceMessage = Just errorMessage
                      }
                    , Cmd.none
                    )


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
            let
                routeInfo =
                    fromUrl url

                shouldLoadSave =
                    case routeInfo.saveId of
                        Just saveId ->
                            model.saveId /= Just saveId && canUsePostgrestModel model

                        Nothing ->
                            False

                updatedModel =
                    { model
                        | url = url
                        , saveId = routeInfo.saveId
                        , loadingSave = False
                        , authMessage = Nothing
                    }
            in
            case routeInfo.saveId of
                Just saveId ->
                    if shouldLoadSave then
                        ( { updatedModel | loadingSave = True }
                        , loadSaveCommand updatedModel saveId
                        )

                    else
                        ( updatedModel, Cmd.none )

                Nothing ->
                    ( updatedModel, Cmd.none )


worldGenerator : Model -> Random.Generator (List Villager)
worldGenerator model =
    pregnancyListGenerator model.villagers
        |> Random.andThen (deathListGenerator (lifeExpectancyBonusYears model))


placeBuilding : Int -> Int -> Model -> Model
placeBuilding cellIndex buildingIndex model =
    case buildingInGridCell cellIndex model.buildingGrid of
        Just _ ->
            { model | draggedBuilding = Nothing }

        Nothing ->
            let
                cost =
                    buildingCost buildingIndex

                canAfford =
                    model.money >= cost

                updatedGrid =
                    setAt cellIndex (Just buildingIndex) model.buildingGrid
            in
            if canAfford then
                refreshDerivedFields
                    { model
                        | buildingGrid = updatedGrid
                        , filledGridRows = filledRowsFromGrid updatedGrid
                        , draggedBuilding = Nothing
                        , money = model.money - cost
                    }

            else
                { model | draggedBuilding = Nothing }


buildingInGridCell : Int -> List (Maybe Int) -> Maybe Int
buildingInGridCell cellIndex buildingGrid =
    buildingGrid
        |> List.drop cellIndex
        |> List.head
        |> Maybe.andThen identity


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
                |> List.map useWater
                |> List.map updatePregnancyDuration
                |> List.concatMap (giveBirth model.newVillager)

        foodPerTick =
            calculateFoodPerTick model

        waterPerTick =
            calculateWaterPerTick model

        moneyPerTick =
            calculateMoneyPerTick model

        producedFood =
            model.food + foodPerTick

        producedWater =
            model.water + waterPerTick

        producedMoney =
            model.money + moneyPerTick

        ( remainingFood, villagersAfterFood ) =
            feedOneRound producedFood processedVillagers

        ( remainingWater, fedVillagers ) =
            feedOneRoundWater producedWater villagersAfterFood
    in
        { model
        | time = model.time + delta
        , food = remainingFood
        , water = remainingWater
        , foodPerTick = foodPerTick
        , waterPerTick = waterPerTick
        , money = producedMoney
        , moneyPerTick = moneyPerTick
        , tick = model.tick + 1
        , villagers = fedVillagers
        , statistics = calculateStatistics fedVillagers
    }


refreshDerivedFields : Model -> Model
refreshDerivedFields model =
    { model
        | filledGridRows = filledRowsFromGrid model.buildingGrid
        , statistics = calculateStatistics model.villagers
        , foodPerTick = calculateFoodPerTick model
        , waterPerTick = calculateWaterPerTick model
        , moneyPerTick = calculateMoneyPerTick model
        , worldCalculationPending = False
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

feedOneRoundWater : Int -> List Villager -> ( Int, List Villager )
feedOneRoundWater availableWater villagers =
    let
        amountToFeed =
            min availableWater (List.length villagers)

        updatedVillagers =
            List.indexedMap
                (\index villager ->
                    if index < amountToFeed then
                        { villager | water = villager.water + 1 }

                    else
                        villager
                )
                villagers
    in
    ( availableWater - amountToFeed, updatedVillagers )


calculateFoodPerTick : Model -> Int
calculateFoodPerTick model =
    20
        + (buildingCount 6 model * 5)
        + (buildingCount 7 model * 5)
        + (buildingCount 4 model * 5)
        + (jobBonusField .food model)


calculateWaterPerTick : Model -> Int
calculateWaterPerTick model =
    20
        + (buildingCount 5 model * 5)
        + (jobBonusField .water model)


calculateMoneyPerTick : Model -> Int
calculateMoneyPerTick model =
    List.length model.villagers
        + (buildingCount 3 model * 10)
        + (jobBonusField .money model)


lifeExpectancyBonusYears : Model -> Int
lifeExpectancyBonusYears model =
    (buildingCount 1 model * 2)
        + (buildingCount 2 model * 3)
        + (jobBonusField .lifeExpectancy model)


buildingCost : Int -> Int
buildingCost buildingIndex =
    case buildingIndex of
        0 ->
            20

        1 ->
            50

        2 ->
            75

        3 ->
            100

        4 ->
            60

        5 ->
            40

        6 ->
            65

        7 ->
            55

        _ ->
            0


buildingCount : Int -> Model -> Int
buildingCount rowIndex model =
    model.filledGridRows
        |> List.drop rowIndex
        |> List.head
        |> Maybe.withDefault 0


type alias JobBonuses =
    { food : Int
    , water : Int
    , money : Int
    , lifeExpectancy : Int
    }


jobBonuses : Model -> JobBonuses
jobBonuses model =
    allJobRows
        |> List.foldl
            (\rowIndex bonuses ->
                let
                    capacity =
                        jobCapacity rowIndex model.filledGridRows

                    workers =
                        model.villagers
                            |> List.filter (\villager -> villager.job == Just rowIndex)
                            |> List.take capacity
                            |> List.length

                    effect =
                        jobEffectForRow rowIndex
                in
                { food = bonuses.food + (workers * effect.food)
                , water = bonuses.water + (workers * effect.water)
                , money = bonuses.money + (workers * effect.money)
                , lifeExpectancy = bonuses.lifeExpectancy + (workers * effect.lifeExpectancy)
                }
            )
            { food = 0, water = 0, money = 0, lifeExpectancy = 0 }


jobBonusField : (JobBonuses -> Int) -> Model -> Int
jobBonusField field model =
    field (jobBonuses model)


toggleJobAssignment : Int -> Int -> Model -> Model
toggleJobAssignment villagerId jobIndex model =
    let
        capacity =
            jobCapacity jobIndex model.filledGridRows

        currentAssignedCount =
            model.villagers
                |> List.filter (\villager -> villager.job == Just jobIndex)
                |> List.length

        assignable =
            currentAssignedCount < capacity

        updateVillager villager =
            if villager.id /= villagerId then
                villager

            else if villager.job == Just jobIndex then
                { villager | job = Nothing }

            else if assignable then
                { villager | job = Just jobIndex }

            else
                villager
    in
    refreshDerivedFields
        { model | villagers = List.map updateVillager model.villagers }


loadSaveCommand : Model -> String -> Cmd Msg
loadSaveCommand model saveId =
    if not (canUsePostgrestModel model) then
        Cmd.none

    else
        Http.request
            { method = "GET"
            , headers = postgrestReadHeaders model
            , url = model.postgrestUrl ++ "/game_saves?id=eq." ++ saveId ++ "&select=id,state"
            , body = Http.emptyBody
            , expect =
                Http.expectJson
                    (SaveLoaded << Result.mapError httpErrorToString)
                    (Decode.list savedGameRowDecoder)
            , timeout = Nothing
            , tracker = Nothing
            }


saveGameCommand : Model -> Cmd Msg
saveGameCommand model =
    case model.saveId of
        Nothing ->
            Cmd.none

        Just saveId ->
            if not (canUsePostgrestModel model) then
                Cmd.none

            else
                Http.request
                    { method =
                        if model.savePersisted then
                            "PATCH"

                        else
                            "POST"
                    , headers = postgrestWriteHeaders model
                    , url =
                        if model.savePersisted then
                            model.postgrestUrl ++ "/game_saves?id=eq." ++ saveId

                        else
                            model.postgrestUrl ++ "/game_saves"
                    , body =
                        if model.savePersisted then
                            Http.jsonBody
                                (Encode.object
                                    [ ( "state", savedGameStateEncoder (savedGameStateFromModel model) )
                                    ]
                                )

                        else
                            Http.jsonBody
                                (Encode.object
                                    [ ( "id", Encode.string saveId )
                                    , ( "state", savedGameStateEncoder (savedGameStateFromModel model) )
                                    ]
                                )
                    , expect =
                        Http.expectWhatever
                            (SaveCompleted << Result.mapError httpErrorToString)
                    , timeout = Nothing
                    , tracker = Nothing
                    }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl message ->
            "Ungültige Save-URL: " ++ message

        Http.Timeout ->
            "Der Speichervorgang hat zu lange gedauert"

        Http.NetworkError ->
            "Keine Verbindung zur Save-API"

        Http.BadStatus status ->
            "Save-API Fehler: " ++ String.fromInt status

        Http.BadBody message ->
            "Ungültige Save-Antwort: " ++ message


canUsePostgrest : Flags -> Bool
canUsePostgrest flags =
    not (String.isEmpty flags.postgrestUrl)


canUsePostgrestModel : Model -> Bool
canUsePostgrestModel model =
    not (String.isEmpty model.postgrestUrl)
        && not (String.isEmpty model.postgrestToken)


postgrestReadHeaders : Model -> List Http.Header
postgrestReadHeaders model =
    postgrestAuthHeaders model
        ++ [ Http.header "Accept-Profile" model.postgrestSchema ]


postgrestWriteHeaders : Model -> List Http.Header
postgrestWriteHeaders model =
    postgrestAuthHeaders model
        ++ [ Http.header "Content-Profile" model.postgrestSchema
           , Http.header "Content-Type" "application/json"
           ]


postgrestAuthHeaders : Model -> List Http.Header
postgrestAuthHeaders model =
    [ Http.header "Authorization" ("Bearer " ++ model.postgrestToken)
    ]


postgrestTokenDecoder : Decode.Decoder LoginResult
postgrestTokenDecoder =
    Decode.map3
        (\ok token maybeError ->
            { ok = ok
            , token = token
            , error = Maybe.withDefault "" maybeError
            }
        )
        (Decode.field "ok" Decode.bool)
        (Decode.field "token" Decode.string)
        (Decode.maybe (Decode.field "error" Decode.string))


generateSaveId : Random.Generator String
generateSaveId =
    Random.int 1000000000 2147483647
        |> Random.map String.fromInt
