module Model exposing (Model)
import Villager exposing (Villager)

type alias Model =
    { time : Float
    , villagers : List Villager
    , tick : Int
    }