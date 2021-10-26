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


type alias Model =
    { typed : List String
    , typing : List String
    }


type Msg
    = NoneYet


initialModel : Model
initialModel =
    let
        randomWords =
            Random.list 6 <| Random.uniform "lucky" corpus

        ( initialList, _ ) =
            Random.step randomWords (Random.initialSeed 1)
    in
    { typing = List.drop 2 initialList, typed = List.take 2 initialList }


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
    , typedFontColor = El.rgb255 140 140 140
    , bgColor = El.rgb255 50 52 55
    , cursor = El.rgb255 222 222 200
    , textSize = 50
    }


view : Model -> Browser.Document Msg
view model =
    { title = "Infinitype"
    , body =
        [ El.layout
            [ Font.color theme.fontColor
            , Font.size theme.textSize
            , Background.color theme.bgColor
            ]
            (El.row [ El.padding 16, El.centerY, El.centerX, El.width <| El.px 1200 ]
                [ El.row [ El.width El.fill, Font.color theme.typedFontColor ] [ El.row [ El.alignRight ] <| renderWords model.typed ]
                , El.el [ Background.color theme.cursor ] (El.text " ")
                , El.row [ El.width El.fill ] <| renderWords model.typing
                ]
            )
        ]
    }
