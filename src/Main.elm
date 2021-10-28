module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Element as El exposing (Element, el)
import Element.Background as Background
import Element.Font as Font
import Json.Decode as D
import List.Extra as LE
import Random exposing (Generator)
import Set exposing (Set)
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
    , heldKeys : Set String
    }


type Msg
    = KeyPressed String
    | KeyReleased String
    | RandomWords (List String)


randomWords : Int -> Generator (List String)
randomWords count =
    Random.list count <| Random.uniform "lucky" corpus


initialModel : Model
initialModel =
    { typing = []
    , typed = []
    , heldKeys = Set.empty
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Random.generate RandomWords (randomWords 500) )


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


handleBackspace : Model -> ( Model, Cmd msg )
handleBackspace model =
    let
        getKey kp =
            case kp of
                Correct key ->
                    key

                Incorrect _ key ->
                    key

        isSuperBackspace =
            List.any (\key -> Set.member key model.heldKeys) [ "Control", "Alt", "Command" ]

        isSpace =
            \char -> getKey char == " "

        isAlphaNum =
            \char -> List.all Char.isAlphaNum <| String.toList (getKey char)

        singleBackspace =
            ( model.typed
                |> List.take (List.length model.typed - 1)
            , model.typed
                |> List.reverse
                |> List.take 1
                |> List.map getKey
            )

        superBackspace =
            let
                remaining =
                    model.typed
                        |> List.reverse
                        |> LE.dropWhile isSpace
                        |> LE.dropWhile isAlphaNum
                        |> List.reverse

                deletedWhitespace =
                    model.typed
                        |> List.reverse
                        |> LE.takeWhile isSpace

                deletedChars =
                    model.typed
                        |> List.reverse
                        |> LE.dropWhile isSpace
                        |> LE.takeWhile isAlphaNum
                        |> List.reverse

                deleted =
                    [ deletedChars, deletedWhitespace ]
                        |> List.concat
                        |> List.map getKey
            in
            ( remaining
            , deleted
            )

        backspaced =
            if isSuperBackspace then
                superBackspace

            else
                singleBackspace

        applyBackspace : Model
        applyBackspace =
            { model
                | typed = Tuple.first backspaced
                , typing = List.concat [ Tuple.second backspaced, model.typing ]
            }
    in
    ( applyBackspace, Cmd.none )


modifiers =
    [ "Control", "Alt", "Shift" ]


handleKeyPressedMsg model key =
    case String.length key of
        1 ->
            handleKeyPressed model key

        _ ->
            case key of
                "Backspace" ->
                    handleBackspace model

                _ ->
                    let
                        modifierPressed =
                            List.member key modifiers

                        updatedModel =
                            if modifierPressed then
                                { model | heldKeys = Set.insert key model.heldKeys }

                            else
                                model
                    in
                    ( updatedModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        KeyPressed key ->
            handleKeyPressedMsg model key

        KeyReleased key ->
            ( { model | heldKeys = Set.remove key model.heldKeys }, Cmd.none )

        RandomWords words ->
            let
                wordsAsChars =
                    List.intersperse " " >> String.join "" >> String.split ""
            in
            ( { model
                | typing = wordsAsChars words
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown keyDownListener
        , onKeyUp keyUpListener
        ]


decodeKey : D.Decoder String
decodeKey =
    D.field "key" D.string


keyDownListener : D.Decoder Msg
keyDownListener =
    D.map (\key -> KeyPressed key) decodeKey


keyUpListener : D.Decoder Msg
keyUpListener =
    D.map (\key -> KeyReleased key) decodeKey


renderLetter : KeyPress -> Element msg
renderLetter keyResult =
    case keyResult of
        Correct key ->
            el [] <| El.text key

        Incorrect actual intended ->
            el [ Font.color theme.incorrect ] <| El.text intended


space : Element msg
space =
    el [] <| El.text " "


renderLetters : List KeyPress -> List (Element msg)
renderLetters words =
    words
        |> List.map renderLetter


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

        recentlyTyped =
            model.typed
                |> List.reverse
                |> List.take 30
                |> List.reverse

        leftColumn =
            El.row [ El.width (El.fill |> El.minimum colWidth), Font.color theme.typedFontColor ] [ El.row [ El.alignRight ] <| renderLetters recentlyTyped ]

        cursor =
            El.el [ Background.color theme.cursor ] (El.text " ")

        rightColumn =
            El.row [ El.width (El.fill |> El.minimum colWidth) ] <| renderLetters (List.map (\key -> Correct key) (List.take 30 model.typing))
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
