module View exposing (view, sidebarContent)

import Constants exposing (gridCellCount, gridCellHeight, gridCellWidth, gridColumns, gridRows)
import Html exposing (Html, a, button, div, p, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Model exposing (Model, SidebarTab(..))
import Msg exposing (Msg(..))
import Svg exposing (Svg, image, rect, svg)
import Svg.Attributes as SvgAttr
import Utils exposing (numberToMonth, tickInYears)
import Villager exposing (viewVillager)
import Url


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

        cellFrame =
            rect
                [ SvgAttr.x (String.fromFloat xPos)
                , SvgAttr.y (String.fromFloat yPos)
                , SvgAttr.width (String.fromFloat gridCellWidth)
                , SvgAttr.height (String.fromFloat gridCellHeight)
                , SvgAttr.fill "#eef5e5"
                , SvgAttr.stroke "#cbd8bd"
                , SvgAttr.strokeWidth "1"
                ]
                []
    in
    if column < filledCellsInRow row model.filledGridRows then
        [ cellFrame
        , image
            [ SvgAttr.x (String.fromFloat xPos)
            , SvgAttr.y (String.fromFloat yPos)
            , SvgAttr.width (String.fromFloat gridCellWidth)
            , SvgAttr.height (String.fromFloat gridCellHeight)
            , SvgAttr.xlinkHref model.tileImage
            , SvgAttr.preserveAspectRatio "xMidYMid slice"
            , SvgAttr.opacity "0.78"
            ]
            []
        ]

    else
        [ cellFrame ]


dashboard : Model -> Html Msg
dashboard model =
    let
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
        , sidebarNavigation model
        , sidebarContent model
        ]


sidebarNavigation : Model -> Html Msg
sidebarNavigation model =
    let
        activeTab =
            tabFromUrl model.url
    in
    div [ HtmlAttr.class "sidebarTabs", HtmlAttr.attribute "role" "tablist" ]
        [ sidebarTabButton activeTab StatisticsTab "Statistiken"
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
                , statCard "Food" (String.fromInt model.food)
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
        "/jobs" ->
            JobsTab
        _ ->
            StatisticsTab


filledCellsInRow : Int -> List Int -> Int
filledCellsInRow targetRow filledGridRows =
    filledGridRows
        |> List.drop targetRow
        |> List.head
        |> Maybe.withDefault 0


rowFillButton : Model -> Int -> Html Msg
rowFillButton model rowIndex =
    let
        filledCells =
            filledCellsInRow rowIndex model.filledGridRows
    in
    button
        [ HtmlAttr.class "fillGridButton"
        , onClick (FillGridRow rowIndex)
        ]
        [ text
            (rowName rowIndex
                ++ " "
                ++ String.fromInt filledCells
                ++ "/"
                ++ String.fromInt gridColumns
            )
        ]


rowName : Int -> String
rowName rowIndex =
    case rowIndex of
        0 ->
            "Houses"

        1 ->
            "Farms"

        2 ->
            "Schools"

        3 ->
            "Markets"

        4 ->
            "Taverns"

        5 ->
            "Wells"

        6 ->
            "Granaries"

        7 ->
            "Bakeries"

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
