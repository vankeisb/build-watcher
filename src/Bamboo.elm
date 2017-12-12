module Bamboo exposing (..)

import Common exposing (..)
import Http exposing (..)
import Task exposing (Task)
import Json.Decode exposing (..)
import Json.Encode as JE


type alias BambooData =
    { serverUrl : String
    , username : String
    , password : String
    , plan : String
    }


type alias BambooResponse =
    { results : BambooResults
    }


type alias BambooResults =
    { result : List BambooResult
    }


type alias BambooResult =
    { buildResultKey : String
    , finished : Bool
    , successful : Bool
    }


type alias BambooValidationErrors =
    { serverUrl : Maybe String
    , plan : Maybe String
    }


canSave : BambooData -> Bool
canSave d =
    not (String.isEmpty d.serverUrl)
        && not (String.isEmpty d.plan)


copy : BambooData -> BambooData
copy d =
    { d
        | plan = d.plan ++ " - copy"
    }


bambooResponseDecoder : Decoder BambooResponse
bambooResponseDecoder =
    map BambooResponse
        (field "results"
            (map BambooResults
                (field "result"
                    (list
                        (map3 BambooResult
                            (field "buildResultKey" string)
                            (field "finished" bool)
                            (field "successful" bool)
                        )
                    )
                )
            )
        )


resultsDecoder : BambooData -> Decoder BuildResult
resultsDecoder bambooData =
    bambooResponseDecoder
        |> andThen (\brs ->
            case bambooResponseToBuildResult bambooData brs of
                Ok br ->
                    succeed br
                Err e ->
                    fail e
        )



bambooResponseToBuildResult : BambooData -> BambooResponse -> Result String BuildResult
bambooResponseToBuildResult data results =
    results.results.result
        |> List.head
        |> Maybe.map (\result ->
            Ok <|
                { url = data.serverUrl ++ "/browse/" ++ result.buildResultKey
                , status =
                    if result.finished then
                        if result.successful then
                            Green
                        else
                            Red
                    else
                        Building
                , name =
                    result.buildResultKey
                }
        )
        |> Maybe.withDefault (Err <| "invalid results ")



fetch : BambooData -> Task Error BuildResult
fetch d =
    let
        authPart =
            if String.isEmpty d.username then
                ""
            else
                "os_authType=basic&os_username="
                ++ encodeUri d.username
                ++ "&os_password="
                ++ encodeUri d.password
                ++ "&"
        u =
            d.serverUrl
                ++ "/rest/api/latest/result/"
                ++ d.plan
                ++ ".json?"
                ++ authPart
                ++ "expand=results[0].result"
        req =
            request
                { method = "GET"
                , headers = []
                , url = u
                , body = emptyBody
                , expect = expectJson (resultsDecoder d)
                , timeout = Nothing
                , withCredentials = False
                }
    in
        toTask req


bambooDataDecoder : Decoder BambooData
bambooDataDecoder =
    map4 BambooData
        (field "serverUrl" string)
        (field "username" stringOrEmpty)
        (field "password" stringOrEmpty)
        (field "plan" string)


encodeBambooData : BambooData -> Value
encodeBambooData v =
    JE.object
        [ ( "serverUrl", JE.string v.serverUrl )
        , ( "username", JE.string v.username )
        , ( "password", JE.string v.password )
        , ( "plan", JE.string v.plan )
        ]
