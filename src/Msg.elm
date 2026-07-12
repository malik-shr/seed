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
    | FillGridRow Int
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
