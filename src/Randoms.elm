module Randoms exposing (pseudoRandomInt)

pseudoRandomInt : Int -> Int -> Int -> Int
pseudoRandomInt min max seed =
    min + modBy (max - min + 1) (abs (seed * 1103515245 + 12345))