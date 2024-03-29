port module Main exposing (..)

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
import Time
import Translations.English as UserText


type alias Corpus =
    { monosize : Float, name : String, words : String }


defaultCorpus : Corpus
defaultCorpus =
    Texts.English1k.corpus


makeCorpus : String -> List String
makeCorpus words =
    words |> String.split "\n" |> List.filter (not << String.isEmpty)


type alias Flags =
    { corpus : Int
    }


type KeyPress
    = Correct String Float
    | Incorrect String String Float
    | Untyped String


type alias AppData =
    { typed : List KeyPress
    , typing : List KeyPress
    , inputValue : String
    , rawText : String
    , stats : StatsData
    , composingInput : Bool
    , heldKeys : Set String
    , corpusData : Corpus
    , shim : Float
    , screen : Maybe Dimensions
    , timeElapsed : Float
    }


type alias StatsData =
    { wpm : String
    , accuracy : String
    , elapsedTime : String
    }


type Model
    = Typing AppData
    | Paused AppData
    | CommandPalette AppData


type CommandName
    = Palette
    | Reset


type Msg
    = InputReceived String
    | Command CommandName
    | ComposingInput Bool
    | KeyDown String
    | KeyReleased String
    | RandomWords (List String)
    | Frame Float
    | Tick Time.Posix
    | NewScreenSize Int Int
    | GotViewport Viewport
    | GrabFocus
    | NoOp


type alias Dimensions =
    { width : Int, height : Int }


randomWords : Int -> List String -> Generator (List String)
randomWords count words =
    let
        fallback =
            Maybe.withDefault "BUG" (List.head words)
    in
    Random.list count <| Random.uniform fallback words


initialData : AppData
initialData =
    { typing = []
    , typed = []
    , heldKeys = Set.empty
    , inputValue = ""
    , rawText = ""
    , stats = StatsData "0" "" ""
    , composingInput = False
    , shim = 0
    , screen = Nothing
    , corpusData = defaultCorpus
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


noOpUpdate : Model -> ( Model, Cmd Msg )
noOpUpdate newModel =
    ( newModel, Cmd.none )


wordBuffer : Int
wordBuffer =
    10


drawMoreWords : Corpus -> Cmd Msg
drawMoreWords corpus =
    corpus.words
        |> makeCorpus
        |> randomWords wordBuffer
        |> Random.generate RandomWords


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialCorpus =
            getCorpus flags.corpus
    in
    ( initialModel
    , Cmd.batch
        [ drawMoreWords initialCorpus
        , Task.perform GotViewport Dom.getViewport
        ]
    )


reset : Model -> ( Model, Cmd Msg )
reset model =
    ( mapModel (\m -> { m | screen = (unwrapModel model).screen }) initialModel
    , Cmd.batch
        [ drawMoreWords (unwrapModel model).corpusData
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



-- UPDATE --


isSpace : Regex.Regex
isSpace =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\s"


resultForKey : Float -> ( String, String ) -> KeyPress
resultForKey timeElapsed ( actual, intended ) =
    if actual == intended || (intended == " " && Regex.contains isSpace actual) then
        Correct actual timeElapsed

    else
        Incorrect actual intended timeElapsed


handleInputReceived : String -> AppData -> ( AppData, List (Cmd Msg) )
handleInputReceived input appData =
    let
        allText =
            List.map getKey <|
                List.concat
                    [ appData.typed
                    , appData.typing
                    ]

        matchingChars =
            List.take (String.length input) allText

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
                        List.map (resultForKey appData.timeElapsed) newChars
                in
                List.concat [ appData.typed, mappedChars ]

        untypedText =
            List.drop (String.length input) allText

        typing =
            List.map Untyped <| untypedText

        difference =
            String.length input - String.length appData.inputValue

        newShim =
            appData.shim + (theme.textSize * appData.corpusData.monosize * toFloat difference)

        untypedWords =
            String.split " " (String.join "" untypedText)

        raw =
            String.dropLeft (String.length appData.inputValue) input

        newData =
            if appData.composingInput then
                { appData
                    | rawText = raw
                }

            else
                { appData
                    | typed = typed
                    , typing = typing
                    , shim = newShim
                    , rawText = ""
                    , inputValue = input
                }

        cmds =
            if List.length untypedWords < wordBuffer then
                [ drawMoreWords appData.corpusData ]

            else
                []
    in
    ( newData, cmds )


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


indexedCorpusList : List ( Int, ( String, Corpus ) )
indexedCorpusList =
    let
        itemsList =
            Dict.toList
                texts

        arrayOfTexts =
            itemsList |> Array.fromList

        items =
            Array.toIndexedList arrayOfTexts
    in
    items


getCorpus : Int -> Corpus
getCorpus idx =
    let
        findFn : ( Int, ( String, Corpus ) ) -> Bool
        findFn ( i, ( _, label ) ) =
            i == idx

        currentCorpus =
            case LE.find findFn indexedCorpusList of
                Just ( _, ( _, corpus ) ) ->
                    corpus

                Nothing ->
                    defaultCorpus
    in
    currentCorpus


incrementCorpus : Int -> Model -> ( Model, Cmd Msg )
incrementCorpus delta model =
    let
        findFn : ( Int, ( String, Corpus ) ) -> Bool
        findFn ( _, ( _, label ) ) =
            label.name == (unwrapModel model).corpusData.name

        currentIndex =
            case LE.find findFn indexedCorpusList of
                Just ( idx, _ ) ->
                    idx

                Nothing ->
                    0

        newIndex =
            modBy (List.length indexedCorpusList) (currentIndex + delta)
    in
    ( mapModel (\appData -> { appData | corpusData = getCorpus newIndex }) model, Cmd.none )


toggleModifier : String -> Model -> ( Model, Cmd Msg )
toggleModifier key model =
    ( mapModel
        (\data ->
            { data
                | heldKeys =
                    if Set.member key data.heldKeys then
                        Set.remove key data.heldKeys

                    else
                        Set.insert key data.heldKeys
            }
        )
        model
    , Cmd.none
    )


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
            label.name == data.corpusData.name

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
    ( Typing
        { data
            | typing = []
            , typed = []
            , inputValue = ""
            , timeElapsed = 0.0
            , corpusData = newCorpus
        }
    , Cmd.batch
        [ drawMoreWords newCorpus
        , refocus
        , corpusChanged newIndex
        ]
    )


handleKeyDown : String -> Model -> ( Model, Cmd Msg )
handleKeyDown key model =
    case model of
        CommandPalette _ ->
            case key of
                "Enter" ->
                    confirmSelection model

                "Escape" ->
                    ( Typing (unwrapModel model), Cmd.none )

                "ArrowUp" ->
                    incrementCorpus -1 model

                "k" ->
                    incrementCorpus -1 model

                "ArrowDown" ->
                    incrementCorpus 1 model

                "j" ->
                    incrementCorpus 1 model

                _ ->
                    ( model, Cmd.none )

        _ ->
            case key of
                "Enter" ->
                    togglePause model

                "Escape" ->
                    commandPalette model

                _ ->
                    ( model, Cmd.none )


animate : Float -> AppData -> AppData
animate dt appData =
    let
        typedKeysAheadOfCursor =
            abs appData.shim / (theme.textSize * appData.corpusData.monosize)

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


refocus : Cmd Msg
refocus =
    Task.attempt (\_ -> NoOp) (Dom.focus "infinitype")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComposingInput val ->
            model
                |> mapModel (\appData -> { appData | composingInput = val, rawText = "" })
                |> noOpUpdate

        InputReceived key ->
            let
                appData =
                    unwrapModel model

                ( newData, cmds ) =
                    appData
                        |> handleInputReceived key
            in
            ( Typing newData
            , Cmd.batch <| cmds ++ [ refocus ]
            )

        Tick _ ->
            ( mapModel calcStats <| model, Cmd.none )

        Command cmd ->
            handleCommand cmd model

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

                lettersToAppend appData =
                    if List.length appData.typing > 0 then
                        " " :: letters

                    else
                        letters

                handler =
                    \appData ->
                        { appData
                            | typing =
                                appData.typing
                                    ++ (List.map Untyped <| lettersToAppend appData)
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

        NewScreenSize w h ->
            model
                |> mapModel
                    (\appData ->
                        { appData
                            | screen =
                                Just
                                    { width = w
                                    , height = h
                                    }
                        }
                    )
                |> noOpUpdate

        GotViewport data ->
            let
                handler =
                    \appData ->
                        { appData
                            | screen =
                                Just
                                    { width = floor <| data.viewport.width
                                    , height = floor <| data.viewport.height
                                    }
                        }
            in
            ( mapModel handler model, refocus )

        GrabFocus ->
            ( model, refocus )

        NoOp ->
            ( model, Cmd.none )


handleCommand : CommandName -> Model -> ( Model, Cmd Msg )
handleCommand cmd model =
    case cmd of
        Palette ->
            commandPalette model

        Reset ->
            reset model



-- PORTS


port corpusChanged : Int -> Cmd msg



-- SUBSCRIPTIONS


port command : (String -> msg) -> Sub msg


commandHandler : String -> Msg
commandHandler cmd =
    case cmd of
        "p" ->
            Command Palette

        "Tab" ->
            Command Reset

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown keyDownListener
        , onAnimationFrameDelta Frame
        , command commandHandler
        , onResize (\w h -> NewScreenSize w h)
        , Time.every 100 Tick
        ]


changeListener : String -> Msg
changeListener key =
    InputReceived key


customEvent : String -> msg -> El.Attribute msg
customEvent evt message =
    El.htmlAttribute <| Html.Events.on evt (D.succeed message)


decodeKey : D.Decoder String
decodeKey =
    D.field "key" D.string


keyDownListener : D.Decoder Msg
keyDownListener =
    D.map KeyDown decodeKey


keyUpListener : D.Decoder Msg
keyUpListener =
    D.map (\key -> KeyReleased key) decodeKey



-- VIEW


type alias Theme =
    { fontColor : El.Color
    , typedFontColor : El.Color
    , bgColor : El.Color
    , incorrect : El.Color
    , incorrectHintColor : El.Color
    , cursor : El.Color
    , veryDim : El.Color
    , textSize : Float
    }


theme : Theme
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
calcStats : AppData -> AppData
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

        stats =
            { wpm = floorStr wpm
            , accuracy = floorStr accuracy ++ "%"
            , elapsedTime = (S.fromInt <| floor time) ++ "s"
            }
    in
    { appData | stats = stats }


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
            , El.height <| El.px (round theme.textSize)
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
            , El.height <| El.px (round theme.textSize)
            , El.alpha 0
            , customEvent "compositionstart" (ComposingInput True)
            , customEvent "compositionend" (ComposingInput False)
            ]
            { text = appData.inputValue ++ appData.rawText
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
                , Font.size <| round theme.textSize // 2
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


renderTypingArea : Model -> Dimensions -> Bool -> Element Msg
renderTypingArea model screen bright =
    let
        appData =
            unwrapModel model

        colWidth =
            screen.width // 2

        widthAttr =
            El.fill |> El.minimum colWidth |> El.maximum colWidth

        themeMonosize =
            theme.textSize * appData.corpusData.monosize

        charCount =
            floor <| ((toFloat screen.width / 2.0) / themeMonosize)

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
            round theme.textSize // 2

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


renderStats : AppData -> Dimensions -> Bool -> Element Msg
renderStats appData screen bright =
    let
        adjustment =
            (toFloat screen.height / 4) - theme.textSize
    in
    el [ El.centerX, El.moveUp adjustment ]
        (El.column []
            [ renderStat ( "wpm", appData.stats.wpm ) bright
            ]
        )


renderPauseHelp : AppData -> Dimensions -> Element Msg
renderPauseHelp appData screens =
    case appData.screen of
        Nothing ->
            El.none

        Just screen ->
            el
                [ El.centerX
                , Font.size <| round theme.textSize // 2
                , El.moveDown <| (toFloat screen.height / 4)
                ]
                (El.column
                    []
                    [ El.text UserText.pauseHint ]
                )


renderTypingHelp : AppData -> Dimensions -> Element Msg
renderTypingHelp appData screen =
    let
        adjustment =
            toFloat screen.height / 5

        hint ( key, value ) =
            El.row [ El.width <| El.px 120 ]
                [ el [ El.width <| El.fillPortion 2 ] (El.text key)
                , el [ El.width <| El.fillPortion 1 ] (El.text value)
                ]
    in
    El.column
        [ El.centerX
        , Font.size <| round theme.textSize // 2
        , Font.color theme.veryDim
        , El.moveDown <| adjustment
        ]
        [ El.column []
            [ hint ( "pause", "⏎" )
            , hint ( "menu ", "␛" )
            , hint ( "reset", "⇥" )
            ]
        ]


renderCommandPalette : Model -> Dimensions -> Element Msg
renderCommandPalette model screen =
    let
        data =
            unwrapModel model

        color name =
            if data.corpusData.name == name then
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
    El.column [ El.width <| El.px <| screen.width ] <|
        List.map
            selectItem
            itemsList


renderComposingHelp : AppData -> Element msg
renderComposingHelp appData =
    let
        help =
            if appData.composingInput && String.length appData.rawText > 0 then
                appData.rawText

            else
                " "

        composingHelp =
            El.row [ El.alignRight, El.centerX, El.moveDown 50 ] [ El.text help ]
    in
    composingHelp


renderStates : Model -> Dimensions -> Element Msg
renderStates model screen =
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
                screen
                bright

        baseAttrs =
            [ El.centerY
            , El.centerX
            , El.width <| El.px (screen.width - 75)
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
                [ renderStats appData screen bright
                , typingLine
                , renderComposingHelp appData
                , renderTypingHelp appData screen
                ]

        width =
            screen.width - 50

        height =
            screen.height

        topCorners =
            El.row [ El.alignBottom ] []

        bottomCorners =
            El.row [ El.alignBottom ] []

        renderState =
            case model of
                CommandPalette _ ->
                    renderCommandPalette model screen

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
            , Font.size <| round theme.textSize
            , Background.color theme.bgColor
            , El.htmlAttribute <| onClick GrabFocus
            ]
            (case (unwrapModel model).screen of
                Nothing ->
                    El.none

                Just screen ->
                    renderStates model screen
            )
        ]
    }
