module View exposing (view)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Utils exposing (tickInYears)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)

import Svg exposing (svg, rect)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)

import Villager exposing (viewVillager)

view : Model -> Html Msg
view model =
    div [class "gameWrapper"]
        [ game model
        , container model
        ]

game: Model -> Html Msg
game model =  
    svg
        [ width "800"
        , height "600"
        , viewBox "0 0 800 600"
        ]
        ([ rect
            [ x "0"
            , y "0"
            , width "800"
            , height "600"
            , fill "#eef5e5"
            ]
            []
            ]
            ++ List.map viewVillager model.villagers
        )

container: Model -> Html Msg
container model = 
    div [class "container"] 
        [ div [] 
            [ p [] [ text ("Tick:" ++ String.fromInt(model.tick))]
            , p [] [ text ("Year:" ++ String.fromInt(tickInYears model.tick))]
            ,p [] [ text ("Month: " ++ String.fromInt (modBy 12 ((model.tick // 15)) + 1))]
            ]
        , div [] 
            [ p [] [ text ("Villagers:" ++ String.fromInt(List.length model.villagers))]
            , p [] [ text ("Dead:" ++ String.fromInt(model.deathCount))]
            ]
        , div [] 
            [ p [] [ text ("Female:" ++ String.fromInt(model.statistics.femaleCount))]
            , p [] [ text ("Male:" ++ String.fromInt(model.statistics.maleCount))]
            , p [] [ text ("Children:" ++ String.fromInt(model.statistics.childrenCount))]
            , p [] [ text ("Adults:" ++ String.fromInt(model.statistics.adultsCount))]
            , p [] [ text ("Pregnant:" ++ String.fromInt(model.statistics.pregnantCount))]
            , p [] [ text ("Fertile Female:" ++ String.fromInt(model.statistics.fertileFemaleCount))]
            ]
    ]
