module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Dict exposing (Dict)
import Element as El exposing (Element, el)
import Element.Background as Background
import Element.Font as Font exposing (monospace)
import Html.Attributes as Attr
import Json.Decode as D
import List.Extra as LE
import Random exposing (Generator)
import Set exposing (Set)
import Texts.English1k as Corpus


corpus : List String
corpus =
    Corpus.words |> String.replace "\n" " " |> String.split " "


charSet : Set Char
charSet =
    Corpus.words |> String.replace "\n" " " |> String.toList |> Set.fromList


type alias Flags =
    {}


type KeyPress
    = Correct String
    | Incorrect String String
    | Untyped String


type alias Model =
    { typed : List KeyPress
    , typing : List KeyPress
    , heldKeys : Set String
    , shim : Float
    , charSet : Set Char
    , charWidthCache : Dict Char Float
    }


type Msg
    = KeyPressed String
    | KeyReleased String
    | RandomWords (List String)
    | Frame Float


randomWords : Int -> Generator (List String)
randomWords count =
    Random.list count <| Random.uniform "lucky" corpus


initialModel : Model
initialModel =
    { typing = []
    , typed = []
    , heldKeys = Set.empty
    , shim = 0
    , charSet = charSet
    , charWidthCache = Dict.empty
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.batch [ Random.generate RandomWords (randomWords 500) ] )


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
                    Untyped ""

        wasAccurate =
            key == getKey nextChar

        result =
            if wasAccurate then
                Correct key

            else
                Incorrect key <| getKey nextChar

        typed =
            List.concat [ model.typed, [ result ] ]

        typing =
            List.drop 1 model.typing

        updatedModel =
            { model | typed = typed, typing = typing, shim = model.shim + themeMonosize }
    in
    ( updatedModel, Cmd.none )


getKey : KeyPress -> String
getKey kp =
    case kp of
        Correct key ->
            key

        Incorrect _ key ->
            key

        Untyped key ->
            key


handleBackspace : Model -> ( Model, Cmd msg )
handleBackspace model =
    let
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
                |> List.map (\k -> Untyped <| getKey k)
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
                        |> List.map (\k -> Untyped <| getKey k)
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
                , shim = model.shim - (toFloat (List.length <| Tuple.second backspaced) * themeMonosize)
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
                | typing = List.map (\k -> Untyped <| k) <| wordsAsChars words
              }
            , Cmd.none
            )

        Frame dt ->
            let
                typedKeysAheadOfCursor =
                    abs model.shim / themeMonosize

                speed =
                    min 4.0 typedKeysAheadOfCursor / 4

                incrementalShim =
                    if model.shim < 0 then
                        model.shim + (dt * speed)

                    else if model.shim > 0 then
                        model.shim - (dt * speed)

                    else
                        model.shim

                updatedShim =
                    if model.shim > 0 && incrementalShim < 0 then
                        0

                    else if model.shim < 0 && incrementalShim > 0 then
                        0

                    else
                        incrementalShim
            in
            ( { model | shim = updatedShim }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown keyDownListener
        , onKeyUp keyUpListener
        , onAnimationFrameDelta Frame
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


renderLetter : Model -> KeyPress -> Element msg
renderLetter model keyResult =
    let
        translateSpaces c =
            if c == ' ' then
                '_'

            else
                c

        mistakeHint actual =
            el
                [ El.centerX
                , Font.color theme.incorrectHintColor
                , Font.size <| theme.textSize // 2
                ]
                (El.text <|
                    String.map
                        translateSpaces
                        actual
                )
    in
    case keyResult of
        Correct key ->
            el
                [ El.moveRight <| model.shim
                , Font.color theme.typedFontColor
                ]
            <|
                El.text key

        Untyped key ->
            el
                [ El.moveRight <| model.shim
                , Font.color theme.fontColor
                , id key
                ]
            <|
                El.text key

        Incorrect actual intended ->
            el
                [ El.moveRight <| model.shim
                , Font.color theme.incorrect
                , El.below <| mistakeHint actual
                ]
            <|
                El.text <|
                    String.map translateSpaces intended


space : Element msg
space =
    el [] <| El.text " "


renderLetters : Model -> List KeyPress -> List (Element msg)
renderLetters model words =
    words
        |> List.map (\l -> renderLetter model l)


themeMonosize =
    theme.textSize * theme.monosize


theme =
    { fontColor = El.rgb255 170 170 170
    , typedFontColor = El.rgba255 140 140 140 0.5
    , bgColor = El.rgb255 17 17 17
    , incorrect = El.rgb255 239 45 86
    , incorrectHintColor = El.rgba255 140 140 140 0.3
    , cursor = El.rgb255 222 222 200
    , textSize = 70
    , monosize = 0.5
    , width = 1600
    }


id : String -> El.Attribute msg
id =
    Attr.id >> El.htmlAttribute


renderTypingArea : Model -> Element msg
renderTypingArea model =
    let
        colWidth =
            (theme.width // 2) - (theme.textSize // 2)

        recentlyTyped =
            model.typed
                |> List.reverse
                |> List.take 16
                |> List.reverse

        leftColumn =
            El.row
                [ El.width (El.fill |> El.minimum colWidth)
                ]
                [ El.row
                    [ El.alignRight
                    ]
                    (renderLetters model recentlyTyped)
                ]

        cursor =
            El.el
                [ Background.color theme.cursor
                , El.width <| El.px 2
                , El.height <| El.px 60
                ]
                (El.text "")

        rightColumn =
            El.row
                [ El.width (El.fill |> El.minimum colWidth)
                ]
                (renderLetters model (List.take 16 model.typing))
    in
    El.row
        []
        [ leftColumn
        , cursor
        , rightColumn
        ]


renderStats model =
    el [ El.centerX, El.moveUp 100 ]
        (El.text
            ""
        )


view : Model -> Browser.Document Msg
view model =
    { title = "Infinitype"
    , body =
        [ El.layout
            [ Font.family [ Font.typeface "infinitype-mono", Font.monospace ]
            , Font.color theme.fontColor
            , Font.size theme.textSize
            , Background.color theme.bgColor
            ]
            (El.row
                [ El.padding 16
                , El.centerY
                , El.centerX
                , El.width <| El.px theme.width
                , El.above <| renderStats model
                ]
                [ renderTypingArea model
                ]
            )
        ]
    }
