module Gitlab exposing (GitlabData, GitlabPipelineData, GitlabValidationErrors, apiUrl, canSave, copy, encodeGitlabData, fetch, gitlabDataDecoder, parseGitlabState, pipelineDecoder, pipelinesDecoder, pipelinesToBuildResult, pipelinesUrl, resultsDecoder)

import Common exposing (..)
import Http exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as JE exposing (..)
import Task exposing (Task)


type alias GitlabData =
    { serverUrl : String
    , project : String
    , branch : String
    , token : String
    }


type alias GitlabPipelineData =
    { status : String
    , ref : String
    , web_url : String
    }


type alias GitlabValidationErrors =
    { serverUrl : Maybe String
    , project : Maybe String
    , branch : Maybe String
    , token : Maybe String
    }


apiUrl : GitlabData -> String
apiUrl data =
    data.serverUrl ++ "/api/v4"


pipelinesUrl : GitlabData -> String
pipelinesUrl data =
    apiUrl data ++ "/projects/" ++ encodeUri data.project ++ "/pipelines?ref=" ++ data.branch


pipelineDecoder : Decoder GitlabPipelineData
pipelineDecoder =
    D.map3 GitlabPipelineData
        (field "status" D.string)
        (field "ref" D.string)
        (field "web_url" D.string)


pipelinesDecoder : D.Decoder (List GitlabPipelineData)
pipelinesDecoder =
    D.list pipelineDecoder


parseGitlabState : String -> Status
parseGitlabState s =
    case s of
        "running" ->
            Building

        "pending" ->
            Building

        "success" ->
            Green

        "failed" ->
            Red

        "canceled" ->
            Unknown

        "skipped" ->
            Unknown

        _ ->
            Unknown


pipelinesToBuildResult : GitlabData -> List GitlabPipelineData -> Result String BuildResult
pipelinesToBuildResult data results =
    results
        |> List.head
        |> Maybe.map
            (\result ->
                Ok <|
                    { url = result.web_url
                    , status = parseGitlabState result.status
                    , name = result.ref
                    }
            )
        |> Maybe.withDefault (Err <| "invalid results ")


resultsDecoder : GitlabData -> Decoder BuildResult
resultsDecoder data =
    pipelinesDecoder
        |> andThen
            (\response ->
                case pipelinesToBuildResult data response of
                    Ok br ->
                        succeed br

                    Err e ->
                        fail e
            )


fetch : GitlabData -> Task Error BuildResult
fetch data =
    let
        req =
            request
                { method = "GET"
                , headers = [ header "PRIVATE-TOKEN" data.token ]
                , url = pipelinesUrl data
                , body = emptyBody
                , expect = expectJson (resultsDecoder data)
                , timeout = Nothing
                , withCredentials = True
                }
    in
    toTask req


canSave : GitlabData -> Bool
canSave d =
    not (String.isEmpty d.serverUrl)
        && not (String.isEmpty d.project)
        && not (String.isEmpty d.branch)
        && not (String.isEmpty d.token)


copy : GitlabData -> GitlabData
copy d =
    { d
        | project = d.project ++ " - copy"
    }


gitlabDataDecoder : Decoder GitlabData
gitlabDataDecoder =
    map4 GitlabData
        (D.field "serverUrl" D.string)
        (D.field "project" D.string)
        (D.field "branch" D.string)
        (D.field "token" D.string)


encodeGitlabData : GitlabData -> List ( String, JE.Value )
encodeGitlabData v =
    [ ( "kind", JE.string "gitlab" )
    , ( "serverUrl", JE.string v.serverUrl )
    , ( "project", JE.string v.project )
    , ( "branch", JE.string v.branch )
    , ( "token", JE.string v.token )
    ]
