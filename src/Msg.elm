module Msg exposing (Msg(..))
import Villager exposing (Villager)

type Msg
    = Tick Float
    | GenNewVillagerValues 
    | NewVillager Villager
    | PregnancyCalculated (List Villager)
    | DeathCalculated (List Villager)
    | FillGridRow Int
