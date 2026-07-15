port module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
import String

import Jobs exposing (allJobRows, assignedWorkerCount, jobCapacity, jobEffectForRow)
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
import Utils exposing (tickInYears)
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
            , jobAssignments = List.repeat (List.length allJobRows) 0
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
            , authPromptOpen = routeInfo.saveId /= Nothing
            , saveId = routeInfo.saveId
            , savePersisted = False
            , saving = False
            , loadingSave = False
            , persistenceMessage =
                case routeInfo.saveId of
                    Just _ ->
                        Just "Spielstand aus dem Link wird vorbereitet..."

                    Nothing ->
                        Nothing
            , key = key
            , url = url
            , worldCalculationPending = False
            , worldCalculationId = 0
            , water = 0
            , waterPerTick = 0
            , money = 0
            , moneyPerTick = 0
            }
        initialCommand =
            case routeInfo.saveId of
                Just saveId ->
                    if canUsePostgrestModel baseModel then
                        loadSaveCommand baseModel saveId

                    else
                        Cmd.none

                Nothing ->
                    Cmd.none
    in
    ( baseModel
    , initialCommand
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
            if model.loadingSave then
                ( model, Cmd.none )

            else
                let
                    updatedModel =
                        updateWorld delta model

                    nextWorldCalculationId =
                        model.worldCalculationId + 1

                    worldCommand =
                        if model.worldCalculationPending then
                            Cmd.none

                        else
                            Random.generate (WorldCalculated nextWorldCalculationId)
                                (worldGenerator updatedModel)

                    nextModel =
                        if model.worldCalculationPending then
                            updatedModel

                        else
                            { updatedModel
                                | worldCalculationPending = True
                                , worldCalculationId = nextWorldCalculationId
                            }
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


        WorldCalculated calculationId updatedVillagers ->
            if model.loadingSave || calculationId /= model.worldCalculationId then
                ( model, Cmd.none )

            else
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

        CloseAuthPrompt ->
            ( { model | authPromptOpen = False, authMessage = Nothing }, Cmd.none )

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
                                    , authPromptOpen = False
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
                ( { model | authMessage = Just "Bitte zuerst anmelden", authPromptOpen = True }, Cmd.none )

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

        LoadRequested ->
            if not (canUsePostgrestModel model) then
                ( { model | authMessage = Just "Bitte zuerst anmelden", authPromptOpen = True }, Cmd.none )

            else
                case (fromUrl model.url).saveId of
                    Just saveId ->
                        ( { model
                            | loadingSave = True
                            , persistenceMessage = Just "Spielstand wird geladen..."
                          }
                        , loadSaveCommand model saveId
                        )

                    Nothing ->
                        ( { model | persistenceMessage = Just "Kein Spielstand-Link vorhanden" }, Cmd.none )

        AssignJobOne jobIndex ->
            ( assignOneAdultToJob jobIndex model, Cmd.none )

        ClearJob jobIndex ->
            ( clearJobAssignments jobIndex model, Cmd.none )

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
                                , worldCalculationId = model.worldCalculationId + 1
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
                        , authMessage =
                            case routeInfo.saveId of
                                Just _ ->
                                    if canUsePostgrestModel model then
                                        Nothing

                                    else
                                        Just "Bitte anmelden, um den Spielstand zu laden"

                                Nothing ->
                                    Nothing
                        , authPromptOpen =
                            case routeInfo.saveId of
                                Just _ ->
                                    not (canUsePostgrestModel model)

                                Nothing ->
                                    False
                        , persistenceMessage =
                            case routeInfo.saveId of
                                Just _ ->
                                    if canUsePostgrestModel model then
                                        Just "Spielstand wird geladen..."

                                    else
                                        Just "Spielstand aus dem Link wird vorbereitet..."

                                Nothing ->
                                    model.persistenceMessage
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
                    model.money >= toFloat(cost)

                updatedGrid =
                    setAt cellIndex (Just buildingIndex) model.buildingGrid
            in
            if canAfford then
                refreshDerivedFields
                    { model
                        | buildingGrid = updatedGrid
                        , filledGridRows = filledRowsFromGrid updatedGrid
                        , draggedBuilding = Nothing
                        , money = model.money - toFloat(cost)
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
            (\buildingIndex ->
                buildingGrid
                    |> List.filterMap
                        (\cell ->
                            if cell == Just buildingIndex then
                                Just buildingIndex

                            else
                                Nothing
                        )
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
    let
        filledGridRows =
            filledRowsFromGrid model.buildingGrid

        statistics =
            calculateStatistics model.villagers

        normalizedJobAssignments =
            normalizeJobAssignments filledGridRows statistics.adultsCount model.jobAssignments

        refreshedModel =
            { model
                | filledGridRows = filledGridRows
                , statistics = statistics
                , jobAssignments = normalizedJobAssignments
                , worldCalculationPending = False
            }
    in
    { refreshedModel
        | foodPerTick = calculateFoodPerTick refreshedModel
        , waterPerTick = calculateWaterPerTick refreshedModel
        , moneyPerTick = calculateMoneyPerTick refreshedModel
    }


adultVillager : Villager -> Bool
adultVillager villager =
    tickInYears villager.age >= 18


freeAdultVillagers : Model -> Int
freeAdultVillagers model =
    max 0 (model.statistics.adultsCount - totalAssignedAdults model)


assignOneAdultToJob : Int -> Model -> Model
assignOneAdultToJob jobIndex model =
    if freeAdultVillagers model <= 0 then
        refreshDerivedFields model

    else
        refreshDerivedFields
            { model
                | jobAssignments =
                    updateJobAssignmentAt jobIndex ((+) 1) model.jobAssignments
            }


clearJobAssignments : Int -> Model -> Model
clearJobAssignments jobIndex model =
    refreshDerivedFields
        { model | jobAssignments = updateJobAssignmentAt jobIndex (\_ -> 0) model.jobAssignments }

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


calculateMoneyPerTick : Model -> Float
calculateMoneyPerTick model =
    toFloat(List.length model.villagers) * 0.0001
        + toFloat(buildingCount 3 model * 10)
        + toFloat(jobBonusField .money model)


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
                        min capacity (assignedWorkerCount rowIndex model.jobAssignments)

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


totalAssignedAdults : Model -> Int
totalAssignedAdults model =
    model.jobAssignments
        |> List.sum


updateJobAssignmentAt : Int -> (Int -> Int) -> List Int -> List Int
updateJobAssignmentAt targetIndex updateCount jobAssignments =
    jobAssignments
        |> List.indexedMap
            (\index count ->
                if index == targetIndex then
                    max 0 (updateCount count)

                else
                    count
            )


normalizeJobAssignments : List Int -> Int -> List Int -> List Int
normalizeJobAssignments filledGridRows adultsCount requestedAssignments =
    let
        requestedWithDefaults =
            List.range 0 (List.length allJobRows - 1)
                |> List.map
                    (\rowIndex ->
                        requestedAssignments
                            |> List.drop rowIndex
                            |> List.head
                            |> Maybe.withDefault 0
                    )

        step rowIndex requested ( remainingAdults, normalizedAssignments ) =
            let
                capacity =
                    jobCapacity rowIndex filledGridRows

                assigned =
                    min requested (min capacity remainingAdults)
            in
            ( remainingAdults - assigned, normalizedAssignments ++ [ assigned ] )
    in
    requestedWithDefaults
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( rowIndex, requested ) acc ->
                step rowIndex requested acc
            )
            ( max 0 adultsCount, [] )
        |> Tuple.second


jobBonusField : (JobBonuses -> Int) -> Model -> Int
jobBonusField field model =
    field (jobBonuses model)


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
