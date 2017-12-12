module Travis exposing (..)

import Common exposing (..)
import Http exposing (..)
import Task exposing (Task)
import Json.Decode exposing (..)
import Json.Encode as JE


type alias TravisData =
    { serverUrl : String
    , token : String
    , repository : String
    , branch : String
    }


type alias BranchResult =
    { lastBuild : LastBuild
    }


type alias LastBuild =
    { state : String
    }


type alias TravisValidationErrors =
    { serverUrl : Maybe String
    , repository : Maybe String
    , branch : Maybe String
    }


canSave : TravisData -> Bool
canSave d =
    not (String.isEmpty d.serverUrl)
        && not (String.isEmpty d.repository)
        && not (String.isEmpty d.branch)


branchResultDecoder : Decoder BranchResult
branchResultDecoder =
    map BranchResult
        (field "last_build"
            (map LastBuild
                (field "state" string)
            )
        )


resultsDecoder : TravisData -> Decoder BuildResult
resultsDecoder data =
    branchResultDecoder
        |> andThen (\branchResult ->
            succeed
                { url =
                    data.serverUrl
                    ++ "/"
                    ++ data.repository
                        |> Debug.log "buildUrl"
                , status =
                    parseTravisState branchResult.lastBuild.state
                , name =
                    data.branch ++ "@" ++ data.repository
                }
        )

parseTravisState : String -> Status
parseTravisState s =
    case s of
        "started" -> Building
        "passed" -> Green
        _ -> Red



fetch : TravisData -> Task Error BuildResult
fetch d =
    let
        apiUrl =
            if d.serverUrl == "https://travis-ci.org" then
                "https://api.travis-ci.org"
            else if d.serverUrl == "https://travis-ci.com" then
                "https://api.travis-ci.com"
            else
                d.serverUrl ++ "/api"
        u =
            apiUrl
                ++ "/repo/"
                ++ encodeUri d.repository
                ++ "/branch/"
                ++ encodeUri d.branch

        authHeader =
            if String.isEmpty d.token then
                []
            else
                [ header "Authorization" ("token " ++ d.token) ]
        req =
            request
                { method = "GET"
                , headers =
                    [ header "Content-Type" "application/json"
                    , header "Accept" "application/json"
                    , header "Travis-API-Version" "3"
                    ] ++ authHeader
                , url = u
                , body = emptyBody
                , expect = expectJson (resultsDecoder d)
                , timeout = Nothing
                , withCredentials = False
                }
    in
        toTask req


travisDataDecoder : Decoder TravisData
travisDataDecoder =
    map4 TravisData
        (field "serverUrl" string)
        (field "token" stringOrEmpty)
        (field "repository" string)
        (field "branch" string)


encodeTravisData : TravisData -> Value
encodeTravisData v =
    JE.object
        [ ( "serverUrl", JE.string v.serverUrl )
        , ( "token", JE.string v.token )
        , ( "repository", JE.string v.repository )
        , ( "branch", JE.string v.branch )
        ]
