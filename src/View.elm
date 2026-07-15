module View exposing (view, sidebarContent)

import Constants exposing (gridCellCount, gridCellHeight, gridCellWidth, gridColumns, gridRows)
import Jobs exposing (allJobRows, assignedWorkerCount, jobCapacity, jobEffectSummary, jobName)
import Html exposing (Html, a, button, div, input, p, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (custom, on, onInput)
import Json.Decode as Decode
import Model exposing (Model, SidebarTab(..))
import Msg exposing (Msg(..))
import Route exposing (shareUrl)
import Villager exposing (Villager, viewVillager)
import Svg exposing (Svg, g, image, rect, svg)
import Svg.Attributes as SvgAttr
import Utils exposing (numberToMonth, tickInYears)
import List


view : Model -> Html Msg
view model =
    div [ HtmlAttr.class "gameWrapper" ]
        [ game model
        , dashboard model
        ]


game : Model -> Html Msg
game model =
    svg
        [ SvgAttr.width "800"
        , SvgAttr.height "600"
        , SvgAttr.viewBox "0 0 800 600"
        , SvgAttr.class "gameCanvas"
        ]
        (grid model
            ++ (model.villagers
                    |> List.take 50
                    |> List.map viewVillager
               )
        )


grid : Model -> List (Svg Msg)
grid model =
    List.range 0 (gridCellCount - 1)
        |> List.concatMap (gridCell model)


gridCell : Model -> Int -> List (Svg Msg)
gridCell model index =
    let
        column =
            modBy gridColumns index

        row =
            index // gridColumns

        xPos =
            toFloat column * gridCellWidth

        yPos =
            toFloat row * gridCellHeight

        droppedBuilding =
            buildingInCell model index

        dropTargetAttributes =
            [ onDragOver
            , onDrop (DropBuilding index)
            ]

        cellFrame =
            rect
                ([ SvgAttr.x (String.fromFloat xPos)
                 , SvgAttr.y (String.fromFloat yPos)
                 , SvgAttr.width (String.fromFloat gridCellWidth)
                 , SvgAttr.height (String.fromFloat gridCellHeight)
                 , SvgAttr.fill "#eef5e5"
                 , SvgAttr.stroke "#cbd8bd"
                 , SvgAttr.strokeWidth "1"
                 ]
                    ++ dropTargetAttributes
                )
                []
    in
    [ g dropTargetAttributes
        (cellFrame
            :: (case droppedBuilding of
                    Just buildingIndex ->
                        [ image
                            ([ SvgAttr.x (String.fromFloat xPos)
                             , SvgAttr.y (String.fromFloat yPos)
                             , SvgAttr.width (String.fromFloat gridCellWidth)
                             , SvgAttr.height (String.fromFloat gridCellHeight)
                             , SvgAttr.xlinkHref (buildingImage model buildingIndex)
                             , SvgAttr.preserveAspectRatio "xMidYMid slice"
                             , SvgAttr.opacity "0.78"
                             ]
                                ++ dropTargetAttributes
                            )
                            []
                        ]

                    Nothing ->
                        []
               )
        )
    ]


buildingInCell : Model -> Int -> Maybe Int
buildingInCell model cellIndex =
    model.buildingGrid
        |> List.drop cellIndex
        |> List.head
        |> Maybe.andThen identity


onDragOver : Svg.Attribute Msg
onDragOver =
    custom "dragover"
        (Decode.succeed
            { message = NoOp
            , stopPropagation = False
            , preventDefault = True
            }
        )


onDrop : Msg -> Svg.Attribute Msg
onDrop msg =
    custom "drop"
        (Decode.succeed
            { message = msg
            , stopPropagation = False
            , preventDefault = True
            }
        )


dashboard : Model -> Html Msg
dashboard model =
    let
        population =
            List.length model.villagers

        routeInfo =
            Route.fromUrl model.url

        levelInfo =
            populationLevelInfo population

        day =
            modBy 30 model.tick + 1

        month =
            modBy 12 (model.tick // 30)

        year =
            tickInYears model.tick
    in
    div [ HtmlAttr.class "dashboard" ]
        [ div [ HtmlAttr.class "dashboardHeader" ]
            [ p [ HtmlAttr.class "eyebrow" ]
                [ text "Seed Simulation" ]
            , p [ HtmlAttr.class "dateText" ]
                [ text
                    (String.fromInt day
                        ++ "."
                        ++ numberToMonth month
                        ++ " "
                        ++ String.fromInt year
                    )
                ]
            ]
        , dashboardSummary model population
        , authPanel model
        , savePanel model routeInfo.tab
        , levelBar levelInfo
        , sidebarNavigation model
        , sidebarContent model
        ]


dashboardSummary : Model -> Int -> Html Msg
dashboardSummary model population =
    div [ HtmlAttr.class "dashboardSummary" ]
        [ summaryCard "Bevölkerung" (String.fromInt population)
        , summaryCard "Geld" (String.fromInt model.money)
        ]


authPanel : Model -> Html Msg
authPanel model =
    div [ HtmlAttr.class "authPanel" ]
        [ div [ HtmlAttr.class "authFieldRow" ]
            [ input
                [ HtmlAttr.class "authInput"
                , HtmlAttr.placeholder "Nutzername"
                , HtmlAttr.value model.postgrestUsername
                , onInput LoginUsernameChanged
                ]
                []
            , input
                [ HtmlAttr.class "authInput"
                , HtmlAttr.placeholder "Passwort"
                , HtmlAttr.type_ "password"
                , HtmlAttr.value model.postgrestPassword
                , onInput LoginPasswordChanged
                ]
                []
            ]
        , button
            [ HtmlAttr.class "authButton"
            , HtmlAttr.disabled model.authInProgress
            , on "click" (Decode.succeed LoginRequested)
            ]
            [ text
                (if model.authInProgress then
                    "Verbinde..."

                 else
                    "Anmelden"
                )
            ]
        , case model.authMessage of
            Just message ->
                p [ HtmlAttr.class "saveMessage" ] [ text message ]

            Nothing ->
                text ""
        ]


savePanel : Model -> SidebarTab -> Html Msg
savePanel model activeTab =
    let
        shareLink =
            case model.saveId of
                Just saveId ->
                    shareUrl model.appBaseUrl (Just saveId) activeTab

                Nothing ->
                    ""

        buttonLabel =
            if model.saving then
                "Speichere..."

            else
                "Spiel speichern"
    in
    div [ HtmlAttr.class "savePanel" ]
        [ button
            [ HtmlAttr.class "saveButton"
            , HtmlAttr.disabled (model.saving || model.loadingSave)
            , on "click" (Decode.succeed SaveRequested)
            ]
            [ text buttonLabel ]
        , case model.saveId of
            Just _ ->
                a [ HtmlAttr.class "saveLink", HtmlAttr.href shareLink ]
                    [ text shareLink ]

            Nothing ->
                span [ HtmlAttr.class "saveLinkPlaceholder" ]
                    [ text "Noch kein Link gespeichert" ]
        , case model.persistenceMessage of
            Just message ->
                p [ HtmlAttr.class "saveMessage" ] [ text message ]

            Nothing ->
                text ""
        ]


summaryCard : String -> String -> Html Msg
summaryCard label value =
    div [ HtmlAttr.class "summaryCard" ]
        [ span [ HtmlAttr.class "summaryLabel" ]
            [ text label ]
        , span [ HtmlAttr.class "summaryValue" ]
            [ text value ]
        ]


levelBar : LevelInfo -> Html Msg
levelBar levelInfo =
    div [ HtmlAttr.class "levelBar" ]
        [ div [ HtmlAttr.class "levelBarHeader" ]
            [ span [ HtmlAttr.class "levelBarLabel" ]
                [ text
                    (if levelInfo.currentLevel == 0 then
                        "Noch kein Level"

                     else
                        "Level " ++ String.fromInt levelInfo.currentLevel
                    )
                ]
            , span [ HtmlAttr.class "levelBarMeta" ]
                [ text
                    (String.fromInt levelInfo.population
                        ++ " / "
                        ++ String.fromInt levelInfo.nextThreshold
                        ++ " Einwohner"
                    )
                ]
            ]
        , div [ HtmlAttr.class "levelBarTrack" ]
            [ div
                [ HtmlAttr.class "levelBarFill"
                , HtmlAttr.style "width"
                    (String.fromFloat (levelInfo.progress * 100) ++ "%")
                ]
                []
            ]
        ]


type alias LevelInfo =
    { population : Int
    , currentLevel : Int
    , nextThreshold : Int
    , progress : Float
    }


populationLevelInfo : Int -> LevelInfo
populationLevelInfo population =
    let
        currentLevel =
            if population < 10 then
                0

            else
                floor (logBase 10 (toFloat population))

        currentThreshold =
            pow10 currentLevel

        nextThreshold =
            pow10 (currentLevel + 1)

        progressRange =
            nextThreshold - currentThreshold

        progress =
            if progressRange <= 0 then
                1

            else
                clamp 0 1
                    (toFloat (population - currentThreshold) / toFloat progressRange)
    in
    { population = population
    , currentLevel = currentLevel
    , nextThreshold = nextThreshold
    , progress = progress
    }


pow10 : Int -> Int
pow10 exponent =
    if exponent <= 0 then
        1

    else
        10 * pow10 (exponent - 1)


sidebarNavigation : Model -> Html Msg
sidebarNavigation model =
    let
        activeTab =
            Route.fromUrl model.url
    in
    div [ HtmlAttr.class "sidebarTabs", HtmlAttr.attribute "role" "tablist" ]
        [ sidebarTabButton model activeTab.tab StatisticsTab "Statistiken"
        , sidebarTabButton model activeTab.tab ProductionTab "Produktion"
        , sidebarTabButton model activeTab.tab JobsTab "Jobs"
        , sidebarTabButton model activeTab.tab BuildingsTab "Häuser"
        ]


sidebarTabButton : Model -> SidebarTab -> SidebarTab -> String -> Html Msg
sidebarTabButton model activeTab tab label =
    let
        isActive =
            activeTab == tab
    in
    a
        [ HtmlAttr.classList
            [ ( "sidebarTab", True )
            , ( "sidebarTabActive", isActive )
            ]
        , HtmlAttr.attribute "role" "tab"
        , HtmlAttr.attribute "aria-selected"
            (if isActive then
                "true"

             else
                "false"
            )
        , HtmlAttr.href (sidebarTabUrl model tab)
        ]
        [ text label ]


sidebarTabUrl : Model -> SidebarTab -> String
sidebarTabUrl model tab =
    shareUrl model.appBaseUrl model.saveId tab


sidebarContent : Model -> Html Msg
sidebarContent model =
    case (Route.fromUrl model.url).tab of
        StatisticsTab ->
            div [ HtmlAttr.class "statsGrid", HtmlAttr.attribute "role" "tabpanel" ]
                [ statCard "Villagers" (String.fromInt (List.length model.villagers))
                , statCard "Dead" (String.fromInt model.deathCount)
                , statCard "Female" (String.fromInt model.statistics.femaleCount)
                , statCard "Male" (String.fromInt model.statistics.maleCount)
                , statCard "Children" (String.fromInt model.statistics.childrenCount)
                , statCard "Adults" (String.fromInt model.statistics.adultsCount)
                , statCard "Pregnant" (String.fromInt model.statistics.pregnantCount)
                , statCard "Fertile Female" (String.fromInt model.statistics.fertileFemaleCount)
                , statCard "Average Age" (String.fromFloat model.statistics.averageAge)
                , statCard "Geld" (String.fromInt model.money)
                ]

        ProductionTab ->
            div [ HtmlAttr.class "productionPanel", HtmlAttr.attribute "role" "tabpanel" ]
                [ p [ HtmlAttr.class "panelTitle" ] [ text "Produktion" ]
                , p [ HtmlAttr.class "panelDescription" ]
                    [ text "Der Bedarf entspricht immer der Anzahl der Villager." ]
                , div [ HtmlAttr.class "productionStack" ]
                    [ resourceRow
                        "Essen"
                        (String.fromInt model.foodPerTick)
                        (String.fromInt (List.length model.villagers))
                        (model.foodPerTick >= List.length model.villagers)
                    , resourceRow
                        "Wasser"
                        (String.fromInt model.waterPerTick)
                        (String.fromInt (List.length model.villagers))
                        (model.waterPerTick >= List.length model.villagers)
                    ]
                ]

        BuildingsTab ->
            div [ HtmlAttr.class "housesPanel", HtmlAttr.attribute "role" "tabpanel" ]
                [ p [ HtmlAttr.class "panelTitle" ] [ text "Gebäude bauen" ]
                , p [ HtmlAttr.class "panelDescription" ]
                    [ text "Wähle eine Gebäudereihe aus, um das Dorf zu erweitern." ]
                , div [ HtmlAttr.class "rowButtonGrid" ]
                    (List.range 0 (gridRows - 1)
                        |> List.map (rowFillButton model)
                    )
                ]

        JobsTab ->
            div [ HtmlAttr.class "jobsPanel", HtmlAttr.attribute "role" "tabpanel" ]
                [ p [ HtmlAttr.class "panelTitle" ] [ text "Jobs zuweisen" ]
                , p [ HtmlAttr.class "panelDescription" ]
                    [ text "Weise Villager vorhandenen Gebäuden zu. Jede Zuweisung gibt dem passenden Stat +1 pro Tick." ]
                , div [ HtmlAttr.class "jobCardGrid" ]
                    (allJobRows |> List.map (jobCard model))
                ]


buildingImage : Model -> Int -> String
buildingImage model rowIndex =
    model.buildingImages
        |> List.drop rowIndex
        |> List.head
        |> Maybe.withDefault model.tileImage


rowFillButton : Model -> Int -> Html Msg
rowFillButton model rowIndex =
    button
        [ HtmlAttr.class "fillGridButton"
        , HtmlAttr.attribute "draggable" "true"
        , on "dragstart" (Decode.succeed (StartDraggingBuilding rowIndex))
        , on "dragend" (Decode.succeed StopDraggingBuilding)
        ]
        [ text (rowName rowIndex) ]


rowName : Int -> String
rowName rowIndex =
    case rowIndex of
        0 ->
            "Häuser (20)"

        1 ->
            "Farmen (50)"

        2 ->
            "Schulen (75)"

        3 ->
            "Kaufhäuser (100)"

        4 ->
            "Tavernen (60)"

        5 ->
            "Brunnen (40)"

        6 ->
            "Getreidespeicher (65)"

        7 ->
            "Bäckereien (55)"

        _ ->
            "Row " ++ String.fromInt (rowIndex + 1)


jobCard : Model -> Int -> Html Msg
jobCard model rowIndex =
    let
        capacity =
            jobCapacity rowIndex model.filledGridRows

        assignedVillagers =
            model.villagers
                |> List.filter (\villager -> villager.job == Just rowIndex)

        availableVillagers =
            model.villagers
                |> List.filter (\villager -> villager.job == Nothing)

        assignedCount =
            min capacity (assignedWorkerCount rowIndex model.villagers)

        canAssign =
            capacity > assignedCount
    in
    div [ HtmlAttr.class "jobCard" ]
        [ div [ HtmlAttr.class "jobCardHeader" ]
            [ span [ HtmlAttr.class "jobCardTitle" ]
                [ text (jobName rowIndex) ]
            , span [ HtmlAttr.class "jobCardMeta" ]
                [ text
                    (String.fromInt assignedCount
                        ++ " / "
                        ++ String.fromInt capacity
                        ++ " Arbeiter"
                    )
                ]
            ]
        , p [ HtmlAttr.class "jobCardDescription" ]
            [ text (jobEffectSummary rowIndex) ]
        , div [ HtmlAttr.class "jobVillagerGroups" ]
            [ jobVillagerGroup "Zugewiesen" assignedVillagers rowIndex True True
            , jobVillagerGroup "Verfügbar" availableVillagers rowIndex canAssign False
            ]
        ]


jobVillagerGroup : String -> List Villager -> Int -> Bool -> Bool -> Html Msg
jobVillagerGroup title villagers rowIndex canAssign isAssignedGroup =
    div [ HtmlAttr.class "jobVillagerGroup" ]
        [ p [ HtmlAttr.class "jobVillagerGroupTitle" ]
            [ text title ]
        , div [ HtmlAttr.class "jobVillagerList" ]
            (case villagers of
                [] ->
                    [ span [ HtmlAttr.class "jobEmptyState" ]
                        [ text
                            (if isAssignedGroup then
                                "Noch niemand zugewiesen"

                             else
                                "Keine freien Villager"
                            )
                        ]
                    ]

                _ ->
                    villagers
                        |> List.map (jobVillagerChip rowIndex canAssign isAssignedGroup)
            )
        ]


jobVillagerChip : Int -> Bool -> Bool -> Villager -> Html Msg
jobVillagerChip rowIndex canAssign isAssignedGroup villager =
    let
        buttonLabel =
            if isAssignedGroup then
                "Entfernen"

            else
                "Zuweisen"

        isDisabled =
            not isAssignedGroup && not canAssign
    in
    div [ HtmlAttr.class "jobVillagerChip" ]
        [ span [ HtmlAttr.class "jobVillagerName" ]
            [ text ("Villager " ++ String.fromInt villager.id) ]
        , button
            [ HtmlAttr.class "jobVillagerButton"
            , HtmlAttr.classList
                [ ( "jobVillagerButtonAssigned", isAssignedGroup )
                , ( "jobVillagerButtonDisabled", isDisabled )
                ]
            , HtmlAttr.disabled isDisabled
            , on "click" (Decode.succeed (ToggleJobAssignment villager.id rowIndex))
            ]
            [ text buttonLabel ]
        ]


statCard : String -> String -> Html Msg
statCard label value =
    div [ HtmlAttr.class "statCard" ]
        [ span [ HtmlAttr.class "statLabel" ]
            [ text label ]
        , span [ HtmlAttr.class "statValue" ]
            [ text value ]
        ]


resourceRow : String -> String -> String -> Bool -> Html Msg
resourceRow label productionValue demandValue isSurplus =
    div [ HtmlAttr.class "resourceRow" ]
        [ p [ HtmlAttr.class "resourceRowTitle" ] [ text label ]
        , div [ HtmlAttr.class "resourceRowGrid" ]
            [ resourceMetric "Produktion" productionValue
                isSurplus
            , demandMetric "Bedarf" demandValue
            ]
        ]


resourceMetric : String -> String -> Bool -> Html Msg
resourceMetric label value isHighlighted =
    div
        [ HtmlAttr.classList
            [ ( "resourceMetric", True )
            , ( "resourceMetricSurplus", isHighlighted )
            , ( "resourceMetricDeficit", not isHighlighted )
            ]
        ]
        [ span [ HtmlAttr.class "resourceMetricLabel" ]
            [ text label ]
        , span [ HtmlAttr.class "resourceMetricValue" ]
            [ text value ]
        ]


demandMetric : String -> String -> Html Msg
demandMetric label value =
    div [ HtmlAttr.class "resourceMetric" ]
        [ span [ HtmlAttr.class "resourceMetricLabel" ]
            [ text label ]
        , span [ HtmlAttr.class "resourceMetricValue" ]
            [ text value ]
        ]
