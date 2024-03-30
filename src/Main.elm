module Main exposing (..)

import Browser
import Model
    exposing
        ( Flags
        , Model(..)
        , Msg(..)
        , init
        )
import Ports exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
