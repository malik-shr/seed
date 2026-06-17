module Utils exposing (tickInYears, numberToMonth, roundTo2)

import Constants exposing (ticksPerYear)

tickInYears : Int -> Int
tickInYears tick =
    tick // ticksPerYear

numberToMonth : Int -> String
numberToMonth monthInNumber =
    if monthInNumber == 0 then 
        "Januar"
    else if monthInNumber == 1 then 
        "Februar"
    else if monthInNumber == 2 then 
        "März"
    else if monthInNumber == 3 then 
        "April"
    else if monthInNumber == 4 then 
        "Mai"
    else if monthInNumber == 5 then 
        "Juni"
    else if monthInNumber == 6 then 
        "Juli"
    else if monthInNumber == 7 then 
        "August"
        else if monthInNumber == 8 then 
        "September"
    else if monthInNumber == 9 then 
        "Oktober"
    else if monthInNumber == 10 then 
        "November"
    else if monthInNumber == 11 then 
        "Dezember"
    else 
        "NaN"

roundTo2 : Float -> Float
roundTo2 value =
    toFloat (round (value * 100)) / 100