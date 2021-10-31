module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Element as El exposing (Element, el)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as D
import List.Extra as LE
import Random exposing (Generator)
import Regex
import Set exposing (Set)
import Task
import Texts.English1k as Corpus



-- import Texts.JapaneseHiraganaCommon as JapaneseCorpus exposing (monosize)


monosize =
    0.5


corpus : List String
corpus =
    Corpus.words |> String.split "\n" |> List.filter (not << String.isEmpty)


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
    , corpus : List String
    , inputValue : String
    , heldKeys : Set String
    , shim : Float
    , screenWidth : Int
    }


type Msg
    = InputReceived String
    | KeyDown String
    | KeyReleased String
    | RandomWords (List String)
    | Frame Float
    | NewScreenSize Int Int
    | GotViewport Viewport
    | GrabFocus
    | NoOp


randomWords : Int -> Generator (List String)
randomWords count =
    Random.list count <| Random.uniform "lucky" corpus


initialModel : Model
initialModel =
    { typing = []
    , typed = []
    , heldKeys = Set.empty
    , inputValue = ""
    , shim = 0
    , screenWidth = 1600
    , corpus = []
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch
        [ Random.generate RandomWords (randomWords 500)
        , Task.perform GotViewport Dom.getViewport
        ]
    )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


isSpace =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\s"


handleInputReceived : Model -> String -> ( Model, Cmd msg )
handleInputReceived model input =
    let
        _ =
            Debug.log "Input: " input

        matchingChars =
            List.take (String.length input) model.corpus

        resultForKey ( actual, intended ) =
            if actual == intended || (intended == " " && Regex.contains isSpace actual) then
                Correct actual

            else
                Incorrect actual <| intended

        keystrokes =
            List.map2 Tuple.pair (String.split "" input) matchingChars

        typed =
            List.map resultForKey keystrokes

        typing =
            List.map Untyped <| List.drop (String.length input) model.corpus

        newShim =
            if String.length input > String.length model.inputValue then
                model.shim + themeMonosize

            else
                model.shim

        updatedModel =
            { model | typed = typed, typing = typing, shim = newShim, inputValue = input }
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

        matchesSpace char =
            Regex.contains isSpace <| getKey char

        isAlphaNum =
            \char -> List.all Char.isAlphaNum <| String.toList (getKey char)

        singleBackspace =
            ( model.typed
                |> List.take (List.length model.typed - 1)
            , model.typed
                |> List.reverse
                |> List.take 1
                |> List.map (getKey >> Untyped)
            )

        superBackspace =
            let
                remaining =
                    model.typed
                        |> List.reverse
                        |> LE.dropWhile matchesSpace
                        |> LE.dropWhile isAlphaNum
                        |> List.reverse

                deletedWhitespace =
                    model.typed
                        |> List.reverse
                        |> LE.takeWhile matchesSpace

                deletedChars =
                    model.typed
                        |> List.reverse
                        |> LE.dropWhile matchesSpace
                        |> LE.takeWhile isAlphaNum
                        |> List.reverse

                deleted =
                    [ deletedChars, deletedWhitespace ]
                        |> List.concat
                        |> List.map (getKey >> Untyped)
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
    [ "Control", "Alt", "Shift", "Super", "Meta" ]


nonTextModifiers =
    [ "Control", "Super", "Meta" ]


preventDefaultKeys =
    [ "'" ]


handleKeyDown : Model -> String -> ( Model, Cmd msg )
handleKeyDown model key =
    case key of
        "Backspace" ->
            handleBackspace model

        -- " " ->
        --     handleInputReceived model " "
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



-- handleKeydownMsg


animate : Model -> Float -> ( Model, Cmd msg )
animate model dt =
    let
        typedKeysAheadOfCursor =
            abs model.shim / themeMonosize

        speed =
            min 4 typedKeysAheadOfCursor / 4

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


refocus =
    Task.attempt (\_ -> NoOp) (Dom.focus "infinitype")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputReceived key ->
            handleInputReceived model key

        KeyReleased key ->
            ( { model | heldKeys = Set.remove key model.heldKeys }, Cmd.none )

        KeyDown key ->
            handleKeyDown model key

        RandomWords words ->
            let
                wordsToLetters =
                    List.intersperse " " >> String.join "" >> String.split ""

                letters =
                    wordsToLetters words
            in
            ( { model
                | typing = List.map Untyped <| letters
                , corpus = letters
              }
            , Cmd.none
            )

        Frame dt ->
            animate model dt

        NewScreenSize w _ ->
            ( { model | screenWidth = w }, Cmd.none )

        GotViewport data ->
            ( { model | screenWidth = floor <| data.viewport.width }, refocus )

        GrabFocus ->
            ( model, refocus )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyUp keyUpListener
        , onKeyDown keyDownListener
        , onAnimationFrameDelta Frame
        , onResize (\w h -> NewScreenSize w h)
        ]


changeListener key =
    InputReceived key


decodeKey : D.Decoder String
decodeKey =
    D.field "key" D.string


keyDownListener : D.Decoder Msg
keyDownListener =
    D.map KeyDown decodeKey


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
    theme.textSize * monosize


theme =
    { fontColor = El.rgb255 170 170 170
    , typedFontColor = El.rgba255 140 140 140 0.5
    , bgColor = El.rgb255 17 17 17
    , incorrect = El.rgb255 239 45 86
    , incorrectHintColor = El.rgba255 140 140 140 0.3
    , cursor = El.rgb255 222 222 200
    , textSize = 50
    }


id : String -> El.Attribute msg
id =
    Attr.id >> El.htmlAttribute


renderTypingArea : Model -> Element Msg
renderTypingArea model =
    let
        colWidth =
            (model.screenWidth // 2) - theme.textSize

        charCount =
            floor <| toFloat model.screenWidth / 2 / themeMonosize

        recentlyTyped =
            model.typed
                |> List.reverse
                |> List.take charCount
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
            El.row []
                [ El.el
                    [ Background.color theme.cursor
                    , El.width <| El.px 2
                    , El.height <| El.px theme.textSize
                    ]
                    El.none
                , Input.text
                    [ Input.focusedOnLoad
                    , id "infinitype"
                    , El.htmlAttribute <| Attr.tabindex 0
                    , El.width <| El.px 1
                    , Background.color theme.cursor
                    , El.alpha 0
                    ]
                    { text = model.inputValue
                    , label = Input.labelHidden ""
                    , onChange = changeListener
                    , placeholder = Nothing
                    }
                ]

        rightColumn =
            El.row
                [ El.width (El.fill |> El.minimum colWidth)
                ]
                (renderLetters model (List.take charCount model.typing))
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
            , El.htmlAttribute <| onClick GrabFocus
            ]
            (El.row
                [ El.padding 16
                , El.centerY
                , El.centerX
                , El.width <| El.px model.screenWidth
                , El.above <| renderStats model
                ]
                [ renderTypingArea model
                ]
            )
        ]
    }
