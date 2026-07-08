module View exposing (view)

import Constants exposing (gridCellCount, gridCellHeight, gridCellWidth, gridColumns, gridRows)
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Svg exposing (Svg, image, rect, svg)
import Svg.Attributes as SvgAttr
import Utils exposing (numberToMonth, tickInYears)
import Villager exposing (viewVillager)


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
                    |> List.take 200
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
        , div [ HtmlAttr.class "statsGrid" ]
            [ statCard "Villagers" (String.fromInt (List.length model.villagers))
            , statCard "Dead" (String.fromInt model.deathCount)
            , statCard "Female" (String.fromInt model.statistics.femaleCount)
            , statCard "Male" (String.fromInt model.statistics.maleCount)
            , statCard "Children" (String.fromInt model.statistics.childrenCount)
            , statCard "Adults" (String.fromInt model.statistics.adultsCount)
            , statCard "Pregnant" (String.fromInt model.statistics.pregnantCount)
            , statCard "Fertile Female" (String.fromInt model.statistics.fertileFemaleCount)
            , statCard "Average Age" (String.fromFloat model.statistics.averageAge)
            ]
        , div [ HtmlAttr.class "rowButtonGrid" ]
            (List.range 0 (gridRows - 1)
                |> List.map (rowFillButton model)
            )
        ]


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
