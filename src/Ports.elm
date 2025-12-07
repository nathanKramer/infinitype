port module Ports exposing (..)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onResize)
import Json.Decode as D
import Model exposing (CommandName(..), Model(..), Msg(..))
import Time



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


decodeKey : D.Decoder String
decodeKey =
    D.field "key" D.string


keyDownListener : D.Decoder Msg
keyDownListener =
    D.map KeyDown decodeKey


keyUpListener : D.Decoder Msg
keyUpListener =
    D.map (\key -> KeyReleased key) decodeKey
