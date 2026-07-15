module View exposing (view, sidebarContent)

import Constants exposing (gridCellCount, gridCellHeight, gridCellWidth, gridColumns, gridRows)
import Html exposing (Html, a, button, div, p, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (custom, on)
import Json.Decode as Decode
import Model exposing (Model, SidebarTab(..))
import Msg exposing (Msg(..))
import Svg exposing (Svg, g, image, rect, svg)
import Svg.Attributes as SvgAttr
import Utils exposing (numberToMonth, tickInYears)
import Villager exposing (viewVillager)
import Url
import List exposing (length)


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
        levelInfo =
            populationLevelInfo (List.length model.villagers)

        day =
            modBy 30 model.tick + 1

        month =
            modBy 12 (model.tick // 30)

        year =
            tickInYears model.tick
    in
    div [ HtmlAttr.class "dashboard" ]
        [ levelBar levelInfo
        , div [ HtmlAttr.class "dashboardHeader" ]
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
        , sidebarNavigation model
        , sidebarContent model
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
            tabFromUrl model.url
    in
    div [ HtmlAttr.class "sidebarTabs", HtmlAttr.attribute "role" "tablist" ]
        [ sidebarTabButton activeTab StatisticsTab "Statistiken"
        , sidebarTabButton activeTab ProductionTab "Produktion"
        , sidebarTabButton activeTab JobsTab "Jobs"
        , sidebarTabButton activeTab BuildingsTab "Häuser"
        ]


sidebarTabButton : SidebarTab -> SidebarTab -> String -> Html Msg
sidebarTabButton activeTab tab label =
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
        , HtmlAttr.href (sidebarTabUrl tab)
        ]
        [ text label ]


sidebarTabUrl : SidebarTab -> String
sidebarTabUrl tab =
    case tab of
        StatisticsTab ->
            "/statistics"
        ProductionTab ->
            "/production"
        JobsTab ->
            "/jobs"
        BuildingsTab ->
            "/buildings"


sidebarContent : Model -> Html Msg
sidebarContent model =
    case tabFromUrl model.url of
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
            div [ HtmlAttr.class "housesPanel", HtmlAttr.attribute "role" "tabpanel" ]
                [ p [ HtmlAttr.class "panelTitle" ] [ text "Gebäude bauen" ]
                , p [ HtmlAttr.class "panelDescription" ]
                    [ text "Wähle eine Gebäudereihe aus, um das Dorf zu erweitern." ]
                , div [ HtmlAttr.class "rowButtonGrid" ]
                    (List.range 0 (gridRows - 1)
                        |> List.map (rowFillButton model)
                    )
                ]


tabFromUrl : Url.Url -> SidebarTab
tabFromUrl url =
    case url.path of
        "/buildings" ->
            BuildingsTab
        "/production" ->
            ProductionTab
        "/jobs" ->
            JobsTab
        _ ->
            StatisticsTab



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
