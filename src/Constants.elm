module Constants exposing (..)

pregnancyChancePerTick : Float
pregnancyChancePerTick =
    0.001

deathAge : Int
deathAge = 85

ticksPerYear : Int 
ticksPerYear = 360

canvasWidth : Float
canvasWidth =
    800

canvasHeight : Float
canvasHeight =
    600

gridColumns : Int
gridColumns =
    10

gridRows : Int
gridRows =
    8

gridCellCount : Int
gridCellCount =
    gridColumns * gridRows

gridCellWidth : Float
gridCellWidth =
    canvasWidth / toFloat gridColumns

gridCellHeight : Float
gridCellHeight =
    canvasHeight / toFloat gridRows
