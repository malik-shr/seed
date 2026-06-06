module Model exposing (Model)
import Villager exposing (Villager)
import Statistics exposing (Statistics)

type alias Model =
    { time : Float
    , villagers : List Villager
    , nextVillagerId : Int
    , tick : Int
    , pregnancyChances : List Int
    , newVillager : Villager
    , deathCount : Int
    , statistics : Statistics
    }