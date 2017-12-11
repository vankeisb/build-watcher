module App exposing (..)

import Html exposing (..)
import Messages exposing (Msg)
import Model exposing (Flags, Model)
import Update
import View

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = Update.init
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
