module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Texts.English1k as Corpus


corpus =
    Corpus.words |> String.replace "\n" " " |> String.split " "


main : Program () Model Msg
main =
    Browser.sandbox { init = Typing corpus, update = update, view = view }


type Model
    = Typing (List String)


type Msg
    = NoneYet


update : Msg -> Model -> Model
update _ model =
    model


renderWord : String -> Html msg
renderWord word =
    Html.span [] [ text word ]


space =
    Html.span [] [ text " " ]


renderWords words =
    words
        |> List.map renderWord
        |> List.intersperse space


view : Model -> Html Msg
view (Typing words) =
    div []
        [ div [] <| renderWords words
        ]
