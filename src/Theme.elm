module Theme exposing (..)

import Element as El


type alias Theme =
    { fontColor : El.Color
    , typedFontColor : El.Color
    , bgColor : El.Color
    , incorrect : El.Color
    , incorrectHintColor : El.Color
    , cursor : El.Color
    , veryDim : El.Color
    , textSize : Float
    }


theme : Theme
theme =
    { fontColor = El.rgb255 170 170 170
    , typedFontColor = El.rgba255 140 140 140 0.5
    , bgColor = El.rgb255 17 17 17
    , incorrect = El.rgb255 239 45 86
    , incorrectHintColor = El.rgba255 140 140 140 0.3
    , cursor = El.rgb255 222 222 200
    , veryDim = El.rgba255 140 140 140 0.3
    , textSize = 50
    }
