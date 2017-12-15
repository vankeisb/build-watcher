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
    , travisToken : Maybe String
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


copy : TravisData -> TravisData
copy d =
    { d
        | branch = d.branch ++ " - copy"
    }


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


apiUrl : String -> String
apiUrl serverUrl =
    if serverUrl == "https://travis-ci.org" then
        "https://api.travis-ci.org"
    else if serverUrl == "https://travis-ci.com" then
        "https://api.travis-ci.com"
    else
        serverUrl ++ "/api"


getToken: TravisData -> Task Error String
getToken d =
    request
      { method = "POST"
      , headers =
          []
      , url = (apiUrl d.serverUrl)
          ++ "/auth/github"
      , body = Http.jsonBody <|
          JE.object
            [ ( "github_token", JE.string d.token)
            ]
      , expect = expectJson travisTokenDecoder
      , timeout = Nothing
      , withCredentials = False
      }
        |> toTask


travisTokenDecoder : Decoder String
travisTokenDecoder =
    field "access_token" string


fetch : TravisData -> Task Error (BuildResult, TravisData)
fetch d =
    let
        createRequest travisToken =
            request
                { method = "GET"
                , headers =
                    [ header "Content-Type" "application/json"
                    , header "Accept" "application/json"
                    , header "Travis-API-Version" "3"
                    ] ++ (
                        if String.isEmpty travisToken then
                            []
                        else
                            [ header "Authorization" ("token " ++ travisToken) ]
                    )
                , url =
                    (apiUrl d.serverUrl)
                        ++ "/repo/"
                        ++ encodeUri d.repository
                        ++ "/branch/"
                        ++ encodeUri d.branch
                , body = emptyBody
                , expect = expectJson (resultsDecoder d)
                , timeout = Nothing
                , withCredentials = False
                }
    in
        case d.travisToken of
            Just token ->
                toTask (createRequest token)
                    |> Task.map (\br -> (br, d))
            Nothing ->
                getToken d
                    |> Task.andThen (\token ->
                        createRequest token
                            |> toTask
                            |> Task.map (\br -> (br, { d | travisToken = Just token }))
                    )


travisDataDecoder : Decoder TravisData
travisDataDecoder =
    map5 TravisData
        (field "serverUrl" string)
        (stringOrEmpty "token")
        (field "repository" string)
        (field "branch" string)
        (succeed Nothing)



encodeTravisData : Bool -> TravisData -> List (String,Value)
encodeTravisData includeCredentials v =
    [ ( "kind", JE.string "travis" )
    , ( "serverUrl", JE.string v.serverUrl )
    , ( "repository", JE.string v.repository )
    , ( "branch", JE.string v.branch )
    ] ++
        if includeCredentials then
            [ ( "token", JE.string v.token ) ]
        else
            []
