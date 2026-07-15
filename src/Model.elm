module Model exposing (Model, SidebarTab(..))

import Browser.Navigation as Nav
import Villager exposing (Villager)
import Statistics exposing (Statistics)
import Url exposing (Url)


type SidebarTab
    = StatisticsTab
    | ProductionTab
    | JobsTab
    | BuildingsTab

type alias Model =
    { time : Float
    , villagers : List Villager
    , nextVillagerId : Int
    , food : Int
    , foodPerTick : Int
    , water : Int
    , waterPerTick : Int 
    , money : Int 
    , moneyPerTick : Int 
    , tick : Int
    , pregnancyChances : List Int
    , newVillager : Villager
    , deathCount : Int
    , statistics : Statistics
    , filledGridRows : List Int
    , buildingGrid : List (Maybe Int)
    , draggedBuilding : Maybe Int
    , tileImage : String
    , buildingImages : List String
    , key : Nav.Key
    , url : Url
    , worldCalculationPending : Bool
    }
