module Jenkins exposing (..)

import Common exposing (..)
import Http exposing (..)
import Task exposing (Task)
import Json.Decode as JD exposing (..)
import Json.Encode as JE


type alias Data =
    { serverUrl : String
    , job : String
    }


canSave : Data -> Bool
canSave d =
    not (String.isEmpty d.serverUrl)
        && not (String.isEmpty d.job)


type alias ValidationErrors =
    { serverUrl : Maybe String
    , job : Maybe String
    }


copy : Data -> Data
copy d =
    { d
        | job = d.job ++ "-copy"
    }


encodeData : Data -> List ( String, JD.Value )
encodeData v =
    [ ( "kind", JE.string "jenkins" )
    , ( "serverUrl", JE.string v.serverUrl )
    , ( "job", JE.string v.job )
    ]


dataDecoder : Decoder Data
dataDecoder =
    map2 Data
        (field "serverUrl" string)
        (field "job" string)


buildUrl : Data -> String
buildUrl data =
    data.serverUrl
        ++ "/job/"
        ++ data.job
        ++ "/lastBuild/api/json?tree=result,url,fullDisplayName"


fetch : Data -> Task Error BuildResult
fetch d =
    let
        u =
            buildUrl d

        req =
            request
                { method = "GET"
                , headers = []
                , url = u
                , body = emptyBody
                , expect = expectJson (decoder d)
                , timeout = Nothing
                , withCredentials = False
                }
    in
        toTask req


decoder : Data -> Decoder BuildResult
decoder data =
    responseDecoder
        |> andThen
            (\brs ->
                case responseToBuildResult data brs of
                    Ok br ->
                        succeed br

                    Err e ->
                        fail e
            )


responseDecoder : Decoder Response
responseDecoder =
    JD.map3 Response
        (field "result" string)
        (field "url" string)
        (field "fullDisplayName" string)


type alias Response =
    { result : String
    , url : String
    , fullDisplayName : String
    }


responseToBuildResult : Data -> Response -> Result String BuildResult
responseToBuildResult data response =
    Ok <|
        { url = response.url
        , status = toStatus response.result
        , name =
            response.fullDisplayName
        }


toStatus : String -> Status
toStatus result =
    case result of
        "SUCCESS" ->
            Green

        "FAILURE" ->
            Red

        "UNSTABLE" ->
            Red

        _ ->
            Unknown



--
--
-- type alias BambooResults =
--     { result : List BambooResult
--     }
--
--
-- type alias BambooResult =
--     { buildResultKey : String
--     , finished : Bool
--     , successful : Bool
--     }
--
--
--
--
--
--
--
--
--
--
--
--
--
-- fetch : BambooData -> Task Error BuildResult
-- fetch d =
--     let
--         authPart =
--             if String.isEmpty d.username then
--                 ""
--             else
--                 "os_authType=basic&os_username="
--                     ++ encodeUri d.username
--                     ++ "&os_password="
--                     ++ encodeUri d.password
--                     ++ "&"
--
--         u =
--             d.serverUrl
--                 ++ "/rest/api/latest/result/"
--                 ++ d.plan
--                 ++ ".json?"
--                 ++ authPart
--                 ++ "expand=results[0].result"
--
--         req =
--             request
--                 { method = "GET"
--                 , headers = []
--                 , url = u
--                 , body = emptyBody
--                 , expect = expectJson (resultsDecoder d)
--                 , timeout = Nothing
--                 , withCredentials = False
--                 }
--     in
--         toTask req
--
--
-- bambooDataDecoder : Decoder BambooData
-- bambooDataDecoder =
--     map4 BambooData
--         (field "serverUrl" string)
--         (stringOrEmpty "username")
--         (stringOrEmpty "password")
--         (field "plan" string)
--
--
-- encodeBambooData : Bool -> BambooData -> List ( String, Value )
-- encodeBambooData includeCredentials v =
--     [ ( "kind", JE.string "bamboo" )
--     , ( "serverUrl", JE.string v.serverUrl )
--     , ( "plan", JE.string v.plan )
--     ]
--         ++ if includeCredentials then
--             [ ( "username", JE.string v.username )
--             , ( "password", JE.string v.password )
--             ]
--            else
--             []
