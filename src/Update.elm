module Update exposing (..)

import Array
import Corpus
    exposing
        ( Corpus
        , defaultCorpus
        , wordBuffer
        )
import Dict
import List.Extra as LE
import Model
    exposing
        ( AppData
        , CommandName(..)
        , KeyPress(..)
        , Model(..)
        , Msg(..)
        , drawMoreWords
        , getAllTexts
        , getCorpus
        , indexedCorpusList
        , mapModel
        , noOpUpdate
        , refocus
        , reset
        , unwrapModel
        )
import Ports exposing (corpusChanged)
import Regex
import Set
import String as S
import Theme exposing (theme)


changeListener : String -> Msg
changeListener key =
    InputReceived key



-- UPDATE --


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

        KeyDown key isComposing ->
            handleKeyDown key isComposing model

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

        hittingBackspace =
            String.length input < List.length appData.typed

        -- Should be one new typed character
        newChars =
            List.drop (List.length appData.typed) keystrokes

        mappedChars =
            List.map (resultForKey appData.timeElapsed) newChars

        typed =
            if hittingBackspace then
                List.take (String.length input) appData.typed

            else
                List.concat [ appData.typed, mappedChars ]

        untypedText =
            List.drop (String.length input) allText

        typing =
            List.map Untyped <| untypedText

        difference =
            String.length input - String.length appData.inputValue

        newShim =
            appData.animationShim + (theme.textSize * appData.corpusData.monosize * toFloat difference)

        untypedWords =
            String.split " " (String.join "" untypedText)

        raw =
            String.dropLeft (String.length appData.inputValue) input

        shouldDrawMoreWords =
            List.length untypedWords < wordBuffer

        cmds =
            if shouldDrawMoreWords then
                [ drawMoreWords appData.corpusData ]

            else
                []

        unwrapMistake keyPress =
            case keyPress of
                Incorrect _ intended _ ->
                    intended /= " "

                _ ->
                    False

        currentWord =
            let
                unTypedString =
                    appData.typing
                        |> List.map getKey
                        |> String.join ""
                        |> String.split " "
                        |> List.head

                typedString =
                    appData.typed
                        |> List.map getKey
                        |> String.join ""
                        |> String.split " "
                        |> List.reverse
                        |> List.head

                theWord =
                    Maybe.withDefault "" typedString
                        ++ Maybe.withDefault "" unTypedString
            in
            theWord

        updatedCorpus corpus =
            let
                corpusWords =
                    String.split "\n" appData.mistakesCorpus.words

                deduped =
                    LE.unique (currentWord :: corpusWords) |> String.join "\n"
            in
            { corpus | words = deduped }

        newMistakesCorpus =
            if
                mappedChars
                    |> List.any unwrapMistake
            then
                updatedCorpus appData.mistakesCorpus

            else
                appData.mistakesCorpus

        newData =
            if appData.composingInput then
                { appData
                    | rawText = raw
                }

            else
                { appData
                    | typed = typed
                    , typing = typing
                    , animationShim = newShim
                    , mistakesCorpus = newMistakesCorpus
                    , rawText = ""
                    , inputValue = input
                }
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


incrementCorpus : Int -> Model -> ( Model, Cmd Msg )
incrementCorpus delta model =
    let
        findFn : ( Int, ( String, Corpus ) ) -> Bool
        findFn ( _, ( _, label ) ) =
            label.name == (unwrapModel model).corpusData.name

        corpusList =
            indexedCorpusList model

        currentIndex =
            case LE.find findFn corpusList of
                Just ( idx, _ ) ->
                    idx

                Nothing ->
                    0

        newIndex =
            modBy (List.length corpusList) (currentIndex + delta)
    in
    ( mapModel (\appData -> { appData | corpusData = getCorpus model newIndex }) model, Cmd.none )


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


confirmSelection : Model -> ( Model, Cmd Msg )
confirmSelection model =
    let
        data =
            unwrapModel model

        itemsList =
            Dict.toList
                (getAllTexts model)

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


handleKeyDown : String -> Bool -> Model -> ( Model, Cmd Msg )
handleKeyDown key isComposing model =
    -- Ignore keydown events during IME composition (e.g., Japanese input)
    -- The Enter key during composition confirms the IME selection, not app actions
    if isComposing then
        ( model, Cmd.none )

    else
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
            abs appData.animationShim / (theme.textSize * appData.corpusData.monosize)

        speed =
            typedKeysAheadOfCursor / 4

        incrementalShim =
            if appData.animationShim < 0 then
                appData.animationShim + (dt * speed)

            else if appData.animationShim > 0 then
                appData.animationShim - (dt * speed)

            else
                appData.animationShim

        updatedShim =
            if appData.animationShim > 0 && incrementalShim < 0 then
                0

            else if appData.animationShim < 0 && incrementalShim > 0 then
                0

            else
                incrementalShim

        updatedTimeElapsed =
            if String.length appData.inputValue > 0 then
                appData.timeElapsed + dt

            else
                appData.timeElapsed
    in
    { appData | animationShim = updatedShim, timeElapsed = updatedTimeElapsed }


handleCommand : CommandName -> Model -> ( Model, Cmd Msg )
handleCommand cmd model =
    case cmd of
        Palette ->
            commandPalette model

        Reset ->
            reset model
