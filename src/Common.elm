module Common exposing (..)

import Json.Decode exposing (..)


type Status
    = Unknown
    | Building
    | Green
    | Red


type alias BuildResult =
    { url : String
    , status : Status
    , name : String
    }


stringOrEmpty : Decoder String
stringOrEmpty =
    oneOf [ string, succeed "" ]
