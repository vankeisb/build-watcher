module GitlabTests exposing (apiUrl, data, pipelineDecoder, pipelineStatus, pipelineStringData, pipelinesBuildResult, pipelinesDecoder, pipelinesStringData, pipelinesUrl)

import Common exposing (..)
import Expect exposing (Expectation)
import Gitlab exposing (..)
import Json.Decode exposing (..)
import Test exposing (..)


data : GitlabData
data =
    { serverUrl = "http://gitlab.com"
    , token = "123456123456"
    , project = "example/elm"
    , branch = "develop"
    }


pipelineStatus : Test
pipelineStatus =
    describe "Check pipelines status to Status"
        [ test "running" <|
            \_ ->
                "running"
                    |> Gitlab.parseGitlabState
                    |> Expect.equal Building
        , test "pending" <|
            \_ ->
                "pending"
                    |> Gitlab.parseGitlabState
                    |> Expect.equal Building
        , test "success" <|
            \_ ->
                "success"
                    |> Gitlab.parseGitlabState
                    |> Expect.equal Green
        , test "failed" <|
            \_ ->
                "failed"
                    |> Gitlab.parseGitlabState
                    |> Expect.equal Red
        , test "canceled" <|
            \_ ->
                "canceled"
                    |> Gitlab.parseGitlabState
                    |> Expect.equal Unknown
        , test "skipped" <|
            \_ ->
                "skipped"
                    |> Gitlab.parseGitlabState
                    |> Expect.equal Unknown
        ]


apiUrl : Test
apiUrl =
    let
        url =
            Gitlab.apiUrl data
    in
    describe "Gitlab api url"
        [ test "Constructs correct api url" <|
            \_ ->
                Expect.equal url "http://gitlab.com/api/v4"
        ]


pipelinesUrl : Test
pipelinesUrl =
    let
        url =
            Gitlab.pipelinesUrl data
    in
    describe "Gitlab pipelines url"
        [ test "constructs the correct pipelines URL" <|
            \_ ->
                Expect.equal url "http://gitlab.com/api/v4/projects/example%2Felm/pipelines?ref=develop"
        ]


pipelineStringData : String
pipelineStringData =
    """
{
    "id": 41875322,
    "sha": "b7211a779ab426b354d2245515e42152861346df",
    "ref": "develop",
    "status": "success",
    "web_url": "https://gitlab.com/example/elm/pipelines/41875322"
}
"""


pipelineDecoder : Test
pipelineDecoder =
    let
        result =
            decodeString Gitlab.pipelineDecoder pipelineStringData
    in
    describe "Gitlab pipeline json decoder"
        [ test "constructs the correct pipeline from json" <|
            \_ ->
                case result of
                    Ok pipeline ->
                        Expect.equal "success" pipeline.status

                    Err _ ->
                        Expect.true ("Error decoding pipeline " ++ pipelineStringData) False
        ]


pipelinesStringData : String
pipelinesStringData =
    """
[
    {
        "id": 44813372,
        "sha": "4423195db41c03e48110d69946bc50fa9ee47565",
        "ref": "develop",
        "status": "pending",
        "web_url": "https://gitlab.com/example/elm/44813372"
    },
    {
        "id": 42736808,
        "sha": "64dde3d4f8b2e8ba24c8b1f972f74d4301503d6b",
        "ref": "develop",
        "status": "failed",
        "web_url": "https://gitlab.com/example/elm/pipelines/42736808"
    },
    {
        "id": 42594925,
        "sha": "528769c0ccc1abe23879c47a2aebbc75ba5fbbef",
        "ref": "develop",
        "status": "canceled",
        "web_url": "https://gitlab.com/example/elm/pipelines/42594925"
    },
    {
        "id": 41875322,
        "sha": "b7211a779ab426b354d2245515e42152861346df",
        "ref": "develop",
        "status": "success",
        "web_url": "https://gitlab.com/example/elm/pipelines/41875322"
    }
]
"""


pipelinesDecoder : Test
pipelinesDecoder =
    let
        result =
            decodeString Gitlab.pipelinesDecoder pipelinesStringData
    in
    describe "Gitlab pipelines json decoder"
        [ test "constructs the correct pipelines pipelinesData from json" <|
            \_ ->
                case result of
                    Ok pipelines ->
                        Expect.equal 4 (List.length pipelines)

                    Err _ ->
                        Expect.true "Error decoding pipelines" False
        ]


pipelinesBuildResult : Test
pipelinesBuildResult =
    let
        results =
            decodeString Gitlab.pipelinesDecoder pipelinesStringData
    in
    describe "Gitlab pipelines to buildResult"
        [ test "get a BuildResult from correct pipelines" <|
            \_ ->
                case results of
                    Err _ ->
                        Expect.true "Error decoding pipelines" False

                    Ok pipelines ->
                        let
                            result =
                                Gitlab.pipelinesToBuildResult data pipelines
                        in
                        case result of
                            Err _ ->
                                Expect.true "Error getting Result" False

                            Ok br ->
                                Expect.equal Building br.status
        ]
