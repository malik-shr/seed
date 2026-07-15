module Msg exposing (LoginResult, Msg(..))

import Browser
import Json.Decode as Decode
import Save exposing (SavedGameRow)
import Villager exposing (Villager)
import Url exposing (Url)

type alias LoginResult =
    { ok : Bool
    , token : String
    , error : String
    }

type Msg
    = Tick Float
    | GenNewVillagerValues 
    | NewVillager Villager
    | WorldCalculated (List Villager)
    | FeedVillagers 
    | ToggleJobAssignment Int Int
    | LoginUsernameChanged String
    | LoginPasswordChanged String
    | LoginRequested
    | PostgrestTokenReceived Decode.Value
    | SaveIdGenerated String
    | SaveRequested
    | SaveCompleted (Result String ())
    | SaveLoaded (Result String (List SavedGameRow))
    | StartDraggingBuilding Int
    | DropBuilding Int
    | StopDraggingBuilding
    | NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
