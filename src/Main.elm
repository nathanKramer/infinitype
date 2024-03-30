module Main exposing (..)

import Browser
import Ports exposing (subscriptions)
import Rendering exposing (view)
import State
    exposing
        ( Flags
        , Model(..)
        , Msg(..)
        , init
        )
import Update exposing (update)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
