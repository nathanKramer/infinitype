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
    = Correct String Float
    | Incorrect String String Float
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


reset =
    ( initialModel
    , Cmd.batch
        [ Random.generate RandomWords (randomWords 500)
        , Task.perform GotViewport Dom.getViewport
        , refocus
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
                Correct actual appData.timeElapsed

            else
                Incorrect actual intended appData.timeElapsed

        keystrokes =
            List.map2 Tuple.pair (String.split "" input) matchingChars

        typed =
            if String.length input < List.length appData.typed then
                List.take (String.length input) appData.typed

            else
                let
                    newChars =
                        List.drop (List.length appData.typed) keystrokes

                    mappedChars =
                        List.map resultForKey newChars
                in
                List.concat [ appData.typed, mappedChars ]

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
        Correct key _ ->
            key

        Incorrect _ key _ ->
            key

        Untyped key ->
            key


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

        "Escape" ->
            reset

        _ ->
            ( model, Cmd.none )


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
            ( Typing
                (model
                    |> unwrapModel
                    |> handleInputReceived key
                )
            , refocus
            )

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


themeMonosize =
    theme.textSize * monosize


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


id : String -> El.Attribute msg
id =
    Attr.id >> El.htmlAttribute


calcStats : AppData -> { wpm : String, accuracy : String, elapsedTime : String }
calcStats appData =
    let
        time =
            appData.timeElapsed / 1000

        rollingPeriod =
            10000.0

        isAfter t key =
            case key of
                Untyped _ ->
                    False

                Correct _ pressTime ->
                    pressTime > t

                Incorrect _ _ pressTime ->
                    pressTime > t

        recentKeyStrokes =
            List.filter (isAfter (appData.timeElapsed - rollingPeriod)) appData.typed

        typedEntries =
            toFloat <| List.length recentKeyStrokes

        rollingPeriodSecs =
            rollingPeriod / 1000.0

        minutes =
            if time < rollingPeriodSecs then
                time / 60.0

            else
                rollingPeriodSecs / 60.0

        isMistake key =
            case key of
                Incorrect _ _ _ ->
                    True

                _ ->
                    False

        mistakes =
            toFloat (List.length <| List.filter isMistake recentKeyStrokes)

        words =
            typedEntries / 5.0

        accuracy =
            (typedEntries - mistakes) / typedEntries * 100

        wpm =
            ((words - mistakes) / minutes)
                |> max 0
                |> min 9999

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
    in
    { wpm = floorStr wpm
    , accuracy = floorStr accuracy ++ "%"
    , elapsedTime = (S.fromInt <| floor time) ++ "s"
    }


renderCursor : Bool -> AppData -> Element Msg
renderCursor bright appData =
    El.row [ El.centerX, El.centerY ]
        [ El.el
            [ Background.color theme.cursor
            , El.width <| El.px 2
            , El.height <| El.px theme.textSize
            , El.alpha
                (if bright then
                    1

                 else
                    0.5
                )
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


renderLetter : KeyPress -> Bool -> AppData -> Element msg
renderLetter keyResult bright appData =
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

        dimmableText color =
            if bright then
                color

            else
                theme.veryDim
    in
    case keyResult of
        Correct key _ ->
            el
                [ El.moveRight <| appData.shim
                , Font.color <| dimmableText theme.typedFontColor
                ]
            <|
                El.text key

        Untyped key ->
            el
                [ El.moveRight <| appData.shim
                , Font.color <| dimmableText theme.fontColor
                ]
            <|
                El.text key

        Incorrect actual intended _ ->
            el
                [ El.moveRight <| appData.shim
                , Font.color <| dimmableText theme.incorrect
                , El.below <| mistakeHint actual
                ]
            <|
                El.text <|
                    String.map translateSpaces intended


renderLetters : AppData -> Bool -> List KeyPress -> List (Element msg)
renderLetters appData bright words =
    words
        |> List.map (\l -> renderLetter l bright appData)


renderTypingArea : Model -> Bool -> Element Msg
renderTypingArea model bright =
    let
        appData =
            unwrapModel model

        colWidth =
            appData.screenWidth // 2

        widthAttr =
            El.fill |> El.minimum colWidth |> El.maximum colWidth

        charCount =
            floor <| ((toFloat appData.screenWidth / 2.0) / themeMonosize)

        recentlyTyped =
            appData.typed
                |> List.reverse
                |> List.take charCount
                |> List.reverse

        renderAppLetters =
            renderLetters appData bright

        leftColumn =
            el
                [ El.width widthAttr
                , El.alpha
                    (if bright then
                        1

                     else
                        0.2
                    )
                ]
                (El.row [ El.alignRight ]
                    (renderAppLetters recentlyTyped)
                )

        rightColumn =
            El.row
                [ El.width widthAttr ]
                (renderAppLetters (List.take charCount appData.typing))
    in
    El.row
        [ El.centerX ]
        [ leftColumn
        , renderCursor bright appData
        , rightColumn
        ]


renderStat : ( String, String ) -> Bool -> Element Msg
renderStat ( statName, value ) bright =
    let
        size =
            theme.textSize // 2

        color =
            theme.typedFontColor

        primaryColor =
            if bright then
                theme.fontColor

            else
                theme.veryDim

        statsSize =
            floor (1.5 * theme.textSize)
    in
    El.column []
        [ el [ Font.size statsSize, Font.color primaryColor ] (El.text value)
        , el
            [ Font.size size
            , Font.color color
            ]
            (El.text statName)
        ]


renderStats : AppData -> Bool -> Element Msg
renderStats appData bright =
    let
        adjustment =
            (toFloat appData.screenHeight / 4) - theme.textSize

        stats =
            calcStats appData
    in
    el [ El.centerX, El.moveUp <| adjustment ]
        (El.column []
            [ renderStat ( "wpm", stats.wpm ) bright
            ]
        )


renderPauseHelp : AppData -> Element Msg
renderPauseHelp appData =
    el
        [ El.centerX
        , Font.size <| theme.textSize // 2
        , El.moveDown (toFloat appData.screenHeight / 4)
        ]
        (El.text "Resume typing to unpause...")


renderTypingHelp : AppData -> Element Msg
renderTypingHelp appData =
    El.column
        [ El.centerX
        , Font.size <| theme.textSize // 2
        , Font.color theme.veryDim
        , El.moveDown (toFloat appData.screenHeight / 4)
        ]
        [ El.text "pause : ⏎", El.text "reset : ␛" ]


renderStates : Model -> Element Msg
renderStates model =
    case model of
        Typing appData ->
            el
                [ El.centerY
                , El.centerX
                , El.width <| El.px appData.screenWidth
                , El.above <| renderStats appData False
                , El.below <| renderTypingHelp appData
                ]
                (renderTypingArea
                    model
                    True
                )

        Paused appData ->
            el
                [ El.centerY
                , El.centerX
                , El.width <| El.px appData.screenWidth
                , El.above <| renderStats appData True
                , El.behindContent (el [ El.centerX, El.moveUp 80.0, Font.size 200 ] (El.text "||"))
                , El.below <| renderPauseHelp appData
                ]
                (renderTypingArea
                    model
                    False
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
            (renderStates model)
        ]
    }
