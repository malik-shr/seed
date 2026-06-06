module Utils exposing (tickInYears)

import Constants exposing (ticksPerYear)

tickInYears : Int -> Int
tickInYears tick =
    tick // ticksPerYear