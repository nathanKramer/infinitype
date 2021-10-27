module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Element as El exposing (Element, el)
import Element.Background as Background
import Element.Font as Font
import Json.Decode as D
import Random
import Texts.English1k as Corpus


corpus : List String
corpus =
    Corpus.words |> String.replace "\n" " " |> String.split " "


type alias Flags =
    {}


type KeyPress
    = Correct String
    | Incorrect String String


type alias Model =
    { typed : List KeyPress
    , typing : List String
    }


type Msg
    = KeyPressed String


initialModel : Model
initialModel =
    let
        randomWords =
            Random.list 6 <| Random.uniform "lucky" corpus

        ( initialList, _ ) =
            Random.step randomWords (Random.initialSeed 1)

        asChars =
            initialList
                |> List.intersperse " "
                |> String.join ""
                |> String.split ""
    in
    { typing = asChars, typed = [] }


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


handleKeyPressed : Model -> String -> ( Model, Cmd msg )
handleKeyPressed model key =
    let
        nextChar =
            case List.take 1 model.typing of
                [ x ] ->
                    x

                _ ->
                    ""

        wasAccurate =
            key == nextChar

        result =
            if wasAccurate then
                Correct key

            else
                Incorrect key nextChar

        typed =
            List.concat [ model.typed, [ result ] ]

        typing =
            List.drop 1 model.typing

        updatedModel =
            { model | typed = typed, typing = typing }
    in
    ( updatedModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        KeyPressed key ->
            case String.length key of
                1 ->
                    handleKeyPressed model key

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDownListener


decodeKey : D.Decoder String
decodeKey =
    D.field "key" D.string


keyDownListener : D.Decoder Msg
keyDownListener =
    D.map (\key -> KeyPressed key) decodeKey


renderWord : KeyPress -> Element msg
renderWord keyResult =
    case keyResult of
        Correct key ->
            el [] <| El.text key

        Incorrect actual intended ->
            el [ Font.color theme.incorrect ] <| El.text intended


space : Element msg
space =
    el [] <| El.text " "


renderWords : List KeyPress -> List (Element msg)
renderWords words =
    words
        |> List.map renderWord


theme =
    { fontColor = El.rgb255 170 170 170
    , typedFontColor = El.rgba255 140 140 140 0.5
    , bgColor = El.rgb255 17 17 17
    , incorrect = El.rgb255 239 45 86
    , cursor = El.rgb255 222 222 200
    , textSize = 50
    , width = 1600
    }


renderTypingArea : Model -> Element msg
renderTypingArea model =
    let
        colWidth =
            (theme.width // 2) - (theme.textSize // 2)

        leftColumn =
            El.row [ El.width (El.fill |> El.minimum colWidth), Font.color theme.typedFontColor ] [ El.row [ El.alignRight ] <| renderWords model.typed ]

        cursor =
            El.el [ Background.color theme.cursor ] (El.text " ")

        rightColumn =
            El.row [ El.width (El.fill |> El.minimum colWidth) ] <| renderWords (List.map (\key -> Correct key) model.typing)
    in
    El.row [ El.padding 16, El.centerY, El.centerX, El.width <| El.px theme.width ]
        [ leftColumn
        , cursor
        , rightColumn
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Infinitype"
    , body =
        [ El.layout
            [ Font.color theme.fontColor
            , Font.size theme.textSize
            , Background.color theme.bgColor
            ]
            (renderTypingArea model)
        ]
    }
