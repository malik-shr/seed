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
    , jobAssignments : List Int
    , nextVillagerId : Int
    , food : Int
    , foodPerTick : Int
    , water : Int
    , waterPerTick : Int 
    , money : Float
    , moneyPerTick : Float 
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
    , appBaseUrl : String
    , postgrestUrl : String
    , postgrestSchema : String
    , postgrestToken : String
    , postgrestUsername : String
    , postgrestPassword : String
    , authInProgress : Bool
    , authMessage : Maybe String
    , authPromptOpen : Bool
    , saveId : Maybe String
    , savePersisted : Bool
    , saving : Bool
    , loadingSave : Bool
    , persistenceMessage : Maybe String
    , key : Nav.Key
    , url : Url
    , worldCalculationPending : Bool
    , worldCalculationId : Int
    }
