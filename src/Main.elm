module Main exposing (..)

import Browser
import Element as El exposing (Element, el)
import Element.Background as Background
import Element.Font as Font
import Random
import Texts.English1k as Corpus


corpus : List String
corpus =
    Corpus.words |> String.replace "\n" " " |> String.split " "


type alias Flags =
    {}


type Model
    = Typing (List String)


type Msg
    = NoneYet


initialModel : Model
initialModel =
    let
        randomWords =
            Random.list 5 <| Random.uniform "lucky" corpus

        ( initialList, _ ) =
            Random.step randomWords (Random.initialSeed 1)
    in
    Typing initialList


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


renderWord : String -> Element msg
renderWord word =
    el [] <| El.text word


space : Element msg
space =
    el [] <| El.text " "


renderWords : List String -> List (Element msg)
renderWords words =
    words
        |> List.map renderWord
        |> List.intersperse space


theme =
    { fontColor = El.rgb255 220 220 220
    , bgColor = El.rgb255 50 52 55
    }


view : Model -> Browser.Document Msg
view (Typing words) =
    { title = "Infinitype"
    , body =
        [ El.layout
            [ Font.color theme.fontColor
            , Font.size 50
            , Background.color theme.bgColor
            ]
            (El.row [ El.padding 16, El.centerY, El.centerX, El.width <| El.px 1200 ]
                [ El.row [ El.width El.fill ] [ El.row [] [] ]
                , El.row [ El.width El.fill, El.alignRight ] <| renderWords words
                ]
            )
        ]
    }
