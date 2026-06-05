module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, p, text)
import Svg exposing (svg, circle, rect)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width, x, y)
import Random

import Model exposing (Model)
import Msg exposing (Msg(..))

import Utils exposing (tickInYears)
import Villager exposing (Villager, viewVillager, breedVillager, giveBirth, ageVillager, moveVillager)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , villagers =
            [ { x = 80, y = 80, vx = 0.2, vy = 0.1, age = 0, gender = 0, isPregnant = False, pregnantDuration = 0 }
            , { x = 150, y = 120, vx = -0.3, vy = 0.5, age = 0, gender = 1, isPregnant = False, pregnantDuration = 0 }
            ]
      , tick = 0
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
            ( updateWorld delta model
            , Cmd.none
            )


updateWorld : Float -> Model -> Model
updateWorld delta model =
    let
        updatedVillagers =
            model.villagers
                |> List.map moveVillager
                |> List.map ageVillager
                |> List.map breedVillager
                |> List.concatMap (giveBirth model.tick)
    in
    { model
        | time = model.time + delta
        , tick = model.tick + 1
        , villagers = updatedVillagers
    }



view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "1280"
            , height "720"
            , viewBox "0 0 1280 720"
            ]
            ([ rect
                [ x "0"
                , y "0"
                , width "1280"
                , height "720"
                , fill "#eef5e5"
                ]
                []
             ]
                ++ List.map viewVillager model.villagers
            )
        , p [] [ text ("Tick:" ++ String.fromInt(model.tick))]
        , p [] [ text ("Villagers:" ++ String.fromInt(List.length model.villagers))]
        ]



