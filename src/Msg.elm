module Msg exposing (Msg(..))

import Browser
import Villager exposing (Villager)
import Url exposing (Url)

type Msg
    = Tick Float
    | GenNewVillagerValues 
    | NewVillager Villager
    | WorldCalculated (List Villager)
    | FeedVillagers 
    | StartDraggingBuilding Int
    | DropBuilding Int
    | StopDraggingBuilding
    | NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
