module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Texts.English1k exposing (words)


main : Program () Model Msg
main =
    Browser.sandbox { init = Typing words, update = update, view = view }


type Model
    = Typing String


type Msg
    = NoneYet


update : Msg -> Model -> Model
update _ model =
    model


view : Model -> Html Msg
view (Typing str) =
    div []
        [ div [] [ text str ]
        ]
