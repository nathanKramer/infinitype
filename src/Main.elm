module Main exposing (..)

import Array
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Dict
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
import Texts.All exposing (texts)
import Texts.English1k
import Translations.English as UserText



-- import Texts.JapaneseHiraganaCommon as JapaneseCorpus exposing (monosize)


monosize =
    0.5


defaultCorpus =
    Texts.English1k.corpus


makeCorpus : String -> List String
makeCorpus words =
    words |> String.split "\n" |> List.filter (not << String.isEmpty)


charSet : Set Char
charSet =
    defaultCorpus.words |> String.replace "\n" " " |> String.toList |> Set.fromList


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
    , corpusName : String
    , shim : Float
    , screenWidth : Int
    , screenHeight : Int
    , timeElapsed : Float
    }


type Model
    = Typing AppData
    | Paused AppData
    | CommandPalette AppData


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


randomWords : Int -> List String -> Generator (List String)
randomWords count words =
    Random.list count <| Random.uniform "lucky" words


initialData : AppData
initialData =
    { typing = []
    , typed = []
    , heldKeys = Set.empty
    , inputValue = ""
    , shim = 0
    , screenWidth = 1600
    , screenHeight = 800
    , corpus = []
    , corpusName = "English 1k"
    , timeElapsed = 0.0
    }


initialModel : Model
initialModel =
    Typing
        initialData


mapModel : (AppData -> AppData) -> Model -> Model
mapModel fn model =
    case model of
        Typing appData ->
            Typing <| fn appData

        Paused appData ->
            Paused <| fn appData

        CommandPalette appData ->
            CommandPalette <| fn appData


unwrapModel : Model -> AppData
unwrapModel model =
    case model of
        Typing appData ->
            appData

        Paused appData ->
            appData

        CommandPalette appData ->
            appData


noOpUpdate newModel =
    ( newModel, Cmd.none )


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch
        [ Random.generate RandomWords (randomWords 500 (makeCorpus Texts.English1k.corpus.words))
        , Task.perform GotViewport Dom.getViewport
        ]
    )


reset : ( Model, Cmd Msg )
reset =
    ( initialModel
    , Cmd.batch
        [ Random.generate RandomWords (randomWords 500 (makeCorpus Texts.English1k.corpus.words))
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

        _ ->
            ( model, refocus )


commandPalette : Model -> ( Model, Cmd Msg )
commandPalette model =
    ( CommandPalette (unwrapModel model), Cmd.none )


incrementCorpus : Int -> Model -> ( Model, Cmd Msg )
incrementCorpus delta model =
    let
        data =
            unwrapModel model

        itemsList =
            Dict.toList
                texts

        itemsArr =
            itemsList |> Array.fromList

        items =
            Array.toIndexedList itemsArr

        findFn : ( Int, ( String, { monosize : Float, name : String, words : String } ) ) -> Bool
        findFn ( _, ( _, label ) ) =
            label.name == data.corpusName

        currentIndex =
            case LE.find findFn items of
                Just ( idx, _ ) ->
                    idx

                Nothing ->
                    0

        newIndex =
            modBy (List.length items) (currentIndex + delta)

        ( newCorpusName, _ ) =
            Maybe.withDefault ( "Lucky Corpus", defaultCorpus ) (Array.get newIndex itemsArr)
    in
    ( mapModel (\appData -> { appData | corpusName = newCorpusName }) model, Cmd.none )


confirmSelection : Model -> ( Model, Cmd Msg )
confirmSelection model =
    let
        data =
            unwrapModel model

        itemsList =
            Dict.toList
                texts

        itemsArr =
            itemsList |> Array.fromList

        items =
            Array.toIndexedList itemsArr

        findFn : ( Int, ( String, { monosize : Float, name : String, words : String } ) ) -> Bool
        findFn ( _, ( _, label ) ) =
            label.name == data.corpusName

        currentIndex =
            case LE.find findFn items of
                Just ( idx, _ ) ->
                    idx

                Nothing ->
                    0

        newIndex =
            modBy (List.length items) currentIndex

        ( _, newCorpus ) =
            Maybe.withDefault ( "Lucky Corpus", defaultCorpus ) (Array.get newIndex itemsArr)
    in
    ( Typing { data | corpus = makeCorpus newCorpus.words }
    , Random.generate RandomWords (randomWords 500 (makeCorpus newCorpus.words))
    )


handleKeyDown : String -> Model -> ( Model, Cmd Msg )
handleKeyDown key model =
    case key of
        "Enter" ->
            case model of
                CommandPalette _ ->
                    confirmSelection model

                _ ->
                    togglePause model

        "Escape" ->
            reset

        "Alt" ->
            commandPalette model

        "ArrowUp" ->
            incrementCorpus -1 model

        "ArrowDown" ->
            incrementCorpus 1 model

        _ ->
            ( model, Cmd.none )


animate : Float -> AppData -> AppData
animate dt appData =
    let
        typedKeysAheadOfCursor =
            abs appData.shim / themeMonosize

        speed =
            typedKeysAheadOfCursor / 4

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

                _ ->
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


{-| Calculate stats like WPM, accuracy, etc.

We might want to think about running this on update only, rather than during rendering.
That would require copying it into our model.

-}
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


{-| Render an input field to capture the user's typing input.
Note that this input needs focus at all times while typing.

To make that happen, we use the `refocus` command.
This happens when the app is initialized, and at other points where focus is disturbed like unpausing.

-}
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
        [ El.centerX, El.centerY ]
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
            , El.centerX
            ]
            (El.text statName)
        ]


renderStats : AppData -> Bool -> Element Msg
renderStats appData bright =
    let
        stats =
            calcStats appData

        adjustment =
            (toFloat appData.screenHeight / 4) - theme.textSize
    in
    el [ El.centerX, El.moveUp adjustment ]
        (El.column []
            [ renderStat ( "wpm", stats.wpm ) bright
            ]
        )


renderPauseHelp : AppData -> Element Msg
renderPauseHelp appData =
    el
        [ El.centerX
        , Font.size <| theme.textSize // 2
        , El.moveDown <| (toFloat appData.screenHeight / 4)
        ]
        (El.column
            []
            [ El.text UserText.pauseHint ]
        )


renderTypingHelp : AppData -> Element Msg
renderTypingHelp appData =
    let
        adjustment =
            toFloat appData.screenHeight / 4

        hint ( key, value ) =
            El.row [ El.width <| El.px 150 ]
                [ el [ El.width <| El.fillPortion 2 ] (El.text key)
                , el [ El.width <| El.fillPortion 1 ] (El.text value)
                ]
    in
    El.column
        [ El.centerX
        , Font.size <| theme.textSize // 2
        , Font.color theme.veryDim
        , El.moveDown <| adjustment
        ]
        [ El.column []
            [ hint ( "Pause", "⏎" )
            , hint ( "reset", "␛" )
            , hint ( "texts", "alt" )
            ]
        ]


renderCommandPalette : Model -> Element Msg
renderCommandPalette model =
    let
        data =
            unwrapModel model

        color name =
            if data.corpusName == name then
                theme.fontColor

            else
                theme.veryDim

        itemsList =
            Dict.toList
                texts

        selectItem ( name, _ ) =
            el
                [ El.centerX
                , Font.color (color name)
                ]
                (El.text name)
    in
    El.column [ El.width <| El.px <| data.screenWidth ] <|
        List.map
            selectItem
            itemsList


renderStates : Model -> Element Msg
renderStates model =
    let
        bright =
            case model of
                Typing _ ->
                    True

                Paused _ ->
                    False

                _ ->
                    True

        appData =
            unwrapModel model

        typingLine =
            renderTypingArea
                model
                bright

        baseAttrs =
            [ El.centerY
            , El.centerX
            , El.width <| El.px appData.screenWidth
            ]

        stateAttrs =
            case model of
                Paused _ ->
                    [ El.behindContent
                        (el
                            [ El.centerX
                            , El.centerY
                            , Font.size 200
                            ]
                            (El.text "||")
                        )
                    ]

                _ ->
                    []

        attrs =
            List.concat
                [ baseAttrs
                , stateAttrs
                ]

        typingArea =
            El.column attrs
                [ renderStats appData bright
                , typingLine
                , renderTypingHelp appData
                ]

        width =
            (unwrapModel model).screenWidth - 50

        height =
            (unwrapModel model).screenHeight

        topCorners =
            El.row [ El.alignBottom ] []

        bottomCorners =
            El.row [ El.alignBottom ] []

        renderState =
            case model of
                CommandPalette _ ->
                    renderCommandPalette model

                _ ->
                    typingArea
    in
    el [] <|
        El.column
            [ El.height <| El.px height ]
            [ el
                [ El.centerX
                , El.width <| El.minimum width <| El.maximum width <| El.fill
                , El.height <| El.fillPortion 1
                ]
                topCorners
            , el [ El.height <| El.fillPortion 6 ] (el [ El.centerY ] renderState)
            , el [ El.height <| El.fillPortion 1 ] bottomCorners
            ]


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
