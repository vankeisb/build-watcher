module App exposing (..)

import Html exposing (..)
import Update
import View
import Model exposing (Model)
import Messages exposing (Msg)

main : Program Never Model Msg
main =
    Html.program
        { init = Update.init
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
