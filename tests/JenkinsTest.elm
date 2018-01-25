module JenkinsTest exposing (..)

import Expect exposing (Expectation)
import Jenkins exposing (..)
import Json.Encode as JE
import Json.Decode as JD
import Test exposing (..)
import Common exposing (Status)


data1 : Data
data1 =
    { serverUrl = "my.url"
    , job = "my-job"
    }


response1 : String
response1 =
    """
{
   "result" : "SUCCESS",
   "_class" : "hudson.maven.MavenModuleSetBuild",
   "url" : "http://my-url/job/my-job/13/",
   "fullDisplayName" : "my-job #13"
}
"""


suite : Test
suite =
    describe "The Jenkins module"
        [ describe "Data"
            -- Nest as many descriptions as you like.
            [ test "can save" <|
                \_ ->
                    canSave data1
                        |> Expect.true "can save"
            , test "can not save" <|
                \_ ->
                    let
                        data =
                            { serverUrl = ""
                            , job = ""
                            }
                    in
                        canSave data
                            |> Expect.false "can not save"
            , test "copy" <|
                \_ ->
                    copy data1
                        |> Expect.equal
                            { serverUrl = "my.url"
                            , job = "my-job-copy"
                            }
            , test "encode" <|
                \_ ->
                    encodeData data1
                        |> Expect.equal
                            [ ( "kind", JE.string "jenkins" )
                            , ( "serverUrl", JE.string "my.url" )
                            , ( "job", JE.string "my-job" )
                            ]
            , test "decode" <|
                \_ ->
                    let
                        encoded =
                            (JE.object << encodeData) data1
                    in
                        JD.decodeValue dataDecoder encoded
                            |> Expect.equal (Ok data1)
            , test "build url" <|
                \_ ->
                    buildUrl data1
                        |> Expect.equal
                            "my.url/job/my-job/lastBuild/api/json?tree=result,url,fullDisplayName"
            ]
        , describe "Fetch"
            [ test "decode" <|
                \_ ->
                    JD.decodeString (decoder data1) response1
                        |> Expect.equal
                            (Result.Ok
                                { url = "http://my-url/job/my-job/13/"
                                , status = Common.Green
                                , name = "my-job #13"
                                }
                            )
            , test "status" <|
                \_ ->
                    [ "SUCCESS"
                    , "FAILURE"
                    , "UNSTABLE"
                    , "other"
                    ]
                        |> List.map toStatus
                        |> Expect.equal
                            [ Common.Green
                            , Common.Red
                            , Common.Red
                            , Common.Unknown
                            ]
            ]
        ]
