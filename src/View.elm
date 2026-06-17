module View exposing (view)

import Html exposing (Html, div, p, span, text)
import Html.Attributes as HtmlAttr

import Model exposing (Model)
import Msg exposing (Msg)

import Svg exposing (rect, svg)
import Svg.Attributes as SvgAttr

import Utils exposing (tickInYears, numberToMonth)

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
        ([ rect
            [ SvgAttr.x "0"
            , SvgAttr.y "0"
            , SvgAttr.width "800"
            , SvgAttr.height "600"
            , SvgAttr.fill "#eef5e5"
            ]
            []
         ]
            ++ (model.villagers
                |> List.take 200
                |> List.map viewVillager
               )
        )


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
                    (
                        String.fromInt day
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
            , statCard "∅Age" (String.fromFloat(model.statistics.averageAge))
            ]
        ]


statCard : String -> String -> Html Msg
statCard label value =
    div [ HtmlAttr.class "statCard" ]
        [ span [ HtmlAttr.class "statLabel" ]
            [ text label ]
        , span [ HtmlAttr.class "statValue" ]
            [ text value ]
        ]