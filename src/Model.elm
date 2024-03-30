module Model exposing (..)

import Browser.Dom as Dom exposing (Viewport)
import Corpus
    exposing
        ( Corpus
        , defaultCorpus
        , getCorpus
        , makeCorpus
        , randomWords
        , wordBuffer
        )
import Random
import Set exposing (Set)
import Task
import Time


type Model
    = Typing AppData
    | Paused AppData
    | CommandPalette AppData


type alias AppData =
    { typed : List KeyPress
    , typing : List KeyPress
    , inputValue : String
    , rawText : String
    , stats : StatsData
    , composingInput : Bool
    , heldKeys : Set String
    , corpusData : Corpus
    , animationShim : Float
    , screen : Maybe Dimensions
    , timeElapsed : Float
    }


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


type alias StatsData =
    { wpm : String
    , accuracy : String
    , elapsedTime : String
    }


type KeyPress
    = Correct String Float
    | Incorrect String String Float
    | Untyped String


type alias Dimensions =
    { width : Int, height : Int }


type CommandName
    = Palette
    | Reset


type alias Flags =
    { corpus : Int
    }


initialData : AppData
initialData =
    { typing = []
    , typed = []
    , heldKeys = Set.empty
    , inputValue = ""
    , rawText = ""
    , stats = StatsData "0" "" ""
    , composingInput = False
    , animationShim = 0
    , screen = Nothing
    , corpusData = defaultCorpus
    , timeElapsed = 0.0
    }


initialModel : Model
initialModel =
    Typing
        initialData


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


refocus : Cmd Msg
refocus =
    Task.attempt (\_ -> NoOp) (Dom.focus "infinitype")


reset : Model -> ( Model, Cmd Msg )
reset model =
    ( mapModel (\m -> { m | screen = (unwrapModel model).screen }) initialModel
    , Cmd.batch
        [ drawMoreWords (unwrapModel model).corpusData
        , Task.perform GotViewport Dom.getViewport
        , refocus
        ]
    )


unwrapModel : Model -> AppData
unwrapModel model =
    case model of
        Typing appData ->
            appData

        Paused appData ->
            appData

        CommandPalette appData ->
            appData


mapModel : (AppData -> AppData) -> Model -> Model
mapModel fn model =
    case model of
        Typing appData ->
            Typing <| fn appData

        Paused appData ->
            Paused <| fn appData

        CommandPalette appData ->
            CommandPalette <| fn appData


noOpUpdate : Model -> ( Model, Cmd Msg )
noOpUpdate newModel =
    ( newModel, Cmd.none )


drawMoreWords : Corpus -> Cmd Msg
drawMoreWords corpus =
    corpus.words
        |> makeCorpus
        |> randomWords wordBuffer
        |> Random.generate RandomWords
