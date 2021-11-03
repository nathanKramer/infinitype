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
import String as S
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


type alias AppData =
    { typed : List KeyPress
    , typing : List KeyPress
    , corpus : List String
    , inputValue : String
    , heldKeys : Set String
    , shim : Float
    , screenWidth : Int
    , screenHeight : Int
    , timeElapsed : Float
    }


type Model
    = Typing AppData
    | Paused AppData


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
    Typing
        { typing = []
        , typed = []
        , heldKeys = Set.empty
        , inputValue = ""
        , shim = 0
        , screenWidth = 1600
        , screenHeight = 800
        , corpus = []
        , timeElapsed = 0.0
        }


mapModel : (AppData -> AppData) -> Model -> Model
mapModel fn model =
    case model of
        Typing appData ->
            Typing <| fn appData

        Paused appData ->
            Paused <| fn appData


unwrapModel : Model -> AppData
unwrapModel model =
    case model of
        Typing appData ->
            appData

        Paused appData ->
            appData


noOpUpdate newModel =
    ( newModel, Cmd.none )


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


handleInputReceived : String -> AppData -> AppData
handleInputReceived input appData =
    let
        matchingChars =
            List.take (String.length input) appData.corpus

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
            List.map Untyped <| List.drop (String.length input) appData.corpus

        difference =
            String.length input - String.length appData.inputValue

        newShim =
            appData.shim + (themeMonosize * toFloat difference)
    in
    { appData | typed = typed, typing = typing, shim = newShim, inputValue = input }


getKey : KeyPress -> String
getKey kp =
    case kp of
        Correct key ->
            key

        Incorrect _ key ->
            key

        Untyped key ->
            key


modifiers =
    [ "Control", "Alt", "Shift", "Super", "Meta" ]


nonTextModifiers =
    [ "Control", "Super", "Meta" ]


preventDefaultKeys =
    [ "'" ]


togglePause : Model -> ( Model, Cmd Msg )
togglePause model =
    case model of
        Typing appData ->
            ( Paused appData, refocus )

        Paused appData ->
            ( Typing appData, refocus )


handleKeyDown : String -> Model -> ( Model, Cmd Msg )
handleKeyDown key model =
    case key of
        "Enter" ->
            togglePause model

        _ ->
            let
                modifierPressed =
                    List.member key modifiers

                updateFn appData =
                    if modifierPressed then
                        { appData | heldKeys = Set.insert key appData.heldKeys }

                    else
                        appData
            in
            model |> mapModel updateFn |> noOpUpdate


animate : Float -> AppData -> AppData
animate dt appData =
    let
        typedKeysAheadOfCursor =
            abs appData.shim / themeMonosize

        speed =
            min 4 typedKeysAheadOfCursor / 4

        incrementalShim =
            if appData.shim < 0 then
                appData.shim + (dt * speed)

            else if appData.shim > 0 then
                appData.shim - (dt * speed)

            else
                appData.shim

        updatedShim =
            if appData.shim > 0 && incrementalShim < 0 then
                0

            else if appData.shim < 0 && incrementalShim > 0 then
                0

            else
                incrementalShim

        updatedTimeElapsed =
            if String.length appData.inputValue > 0 then
                appData.timeElapsed + dt

            else
                appData.timeElapsed
    in
    { appData | shim = updatedShim, timeElapsed = updatedTimeElapsed }


refocus =
    Task.attempt (\_ -> NoOp) (Dom.focus "infinitype")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputReceived key ->
            model
                |> mapModel (handleInputReceived key)
                |> noOpUpdate

        KeyReleased key ->
            model
                |> mapModel (\appData -> { appData | heldKeys = Set.remove key appData.heldKeys })
                |> noOpUpdate

        KeyDown key ->
            handleKeyDown key model

        RandomWords words ->
            let
                wordsToLetters =
                    List.intersperse " " >> String.join "" >> String.split ""

                letters =
                    wordsToLetters words

                handler =
                    \appData ->
                        { appData
                            | typing = List.map Untyped <| letters
                            , corpus = letters
                        }
            in
            model
                |> mapModel handler
                |> noOpUpdate

        Frame dt ->
            case model of
                Typing appData ->
                    noOpUpdate <| Typing (animate dt appData)

                Paused _ ->
                    noOpUpdate model

        NewScreenSize w _ ->
            model
                |> mapModel
                    (\appData -> { appData | screenWidth = w })
                |> noOpUpdate

        GotViewport data ->
            let
                handler =
                    \appData -> { appData | screenWidth = floor <| data.viewport.width, screenHeight = floor <| data.viewport.height }
            in
            ( mapModel handler model, refocus )

        GrabFocus ->
            ( model, refocus )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown keyDownListener
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


renderLetter : KeyPress -> AppData -> Element msg
renderLetter keyResult appData =
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
                [ El.moveRight <| appData.shim
                , Font.color theme.typedFontColor
                ]
            <|
                El.text key

        Untyped key ->
            el
                [ El.moveRight <| appData.shim
                , Font.color theme.fontColor
                ]
            <|
                El.text key

        Incorrect actual intended ->
            el
                [ El.moveRight <| appData.shim
                , Font.color theme.incorrect
                , El.below <| mistakeHint actual
                ]
            <|
                El.text <|
                    String.map translateSpaces intended


space : Element msg
space =
    el [] <| El.text " "


renderLetters : List KeyPress -> AppData -> List (Element msg)
renderLetters words appData =
    words
        |> List.map (\l -> renderLetter l appData)


themeMonosize =
    theme.textSize * monosize


theme =
    { fontColor = El.rgb255 170 170 170
    , typedFontColor = El.rgba255 140 140 140 0.5
    , bgColor = El.rgb255 17 17 17
    , incorrect = El.rgb255 239 45 86
    , incorrectHintColor = El.rgba255 140 140 140 0.3
    , cursor = El.rgb255 222 222 200
    , statsColor = El.rgba255 140 140 140 0.3
    , textSize = 50
    }


id : String -> El.Attribute msg
id =
    Attr.id >> El.htmlAttribute


renderTypingArea : Model -> Element Msg
renderTypingArea model =
    let
        appData =
            unwrapModel model

        colWidth =
            (appData.screenWidth // 2) - theme.textSize

        charCount =
            floor <| toFloat appData.screenWidth / 2 / themeMonosize

        recentlyTyped =
            appData.typed
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
                    (renderLetters recentlyTyped appData)
                ]

        cursor =
            El.row [ El.centerY ]
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
                    , El.height <| El.px theme.textSize
                    , El.alpha 0
                    ]
                    { text = appData.inputValue
                    , label = Input.labelHidden ""
                    , onChange = changeListener
                    , placeholder = Nothing
                    }
                ]

        rightColumn =
            El.row
                [ El.width (El.fill |> El.minimum colWidth)
                ]
                (renderLetters (List.take charCount appData.typing) appData)
    in
    El.row
        []
        [ leftColumn
        , cursor
        , rightColumn
        ]


renderWpm : AppData -> Element msg
renderWpm appData =
    let
        time =
            appData.timeElapsed / 1000

        typedEntries =
            toFloat <| String.length appData.inputValue

        minutes =
            time / 60.0

        isMistake key =
            case key of
                Incorrect _ _ ->
                    True

                _ ->
                    False

        mistakes =
            toFloat (List.length <| List.filter isMistake appData.typed)

        words =
            typedEntries / 5.0

        accuracy =
            (typedEntries - mistakes) / typedEntries * 100

        wpm =
            (words - mistakes) / minutes

        rejectNaN f =
            if isNaN f then
                0.0

            else
                f

        floorStr f =
            f
                |> rejectNaN
                |> floor
                |> S.fromInt

        wpmString =
            "WPM: "
                ++ floorStr wpm

        accuracyString =
            "Accuracy: " ++ floorStr accuracy ++ "%"

        elapsedTimeString =
            "Time: " ++ (S.fromInt <| floor time) ++ "s"

        output =
            [ wpmString
            , accuracyString
            , elapsedTimeString
            ]
    in
    El.text <| String.join ", " output


renderStats : AppData -> List (El.Attribute Msg) -> Element Msg
renderStats appData attrs =
    let
        adjustment =
            (toFloat appData.screenHeight / 4) - theme.textSize
    in
    el [ El.centerX, El.moveUp <| adjustment ]
        (El.row
            (List.concat
                [ attrs, [ Font.size <| floor (theme.textSize * 0.75) ] ]
            )
            [ renderWpm appData
            ]
        )


renderStates : Model -> Element Msg
renderStates model =
    case model of
        Typing appData ->
            el
                [ El.centerY
                , El.centerX
                , El.width <| El.px appData.screenWidth
                , El.above <| renderStats appData [ Font.color theme.statsColor ]
                ]
                (renderTypingArea
                    model
                )

        Paused appData ->
            el
                [ El.centerY
                , El.centerX
                , El.width <| El.px appData.screenWidth
                , El.above <| renderStats appData [ Font.color theme.fontColor ]
                ]
                (el [ El.centerX ] (El.text "||"))


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
            (renderStates model)
        ]
    }
