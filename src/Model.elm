module Model exposing (..)

import Bamboo exposing (BambooData)
import Travis exposing (TravisData)
import Common exposing (..)
import Time exposing (Time)
import Http exposing (Error)
import Material
import Json.Decode exposing (..)
import Json.Encode as JE
import Material.Snackbar as Snackbar

type alias Flags =
    { appName : String
    , appVersion : String
    }

type BuildDef
    = BambooDef BambooData
    | TravisDef TravisData


type alias Build =
    { def : BuildDef
    , result : Maybe BuildResult
    , lastFetch : Time
    , fetching : Bool
    , fetchError : Maybe Http.Error
    , previousStatus : Status
    }


defaultBuild : BuildDef -> Build
defaultBuild buildDef =
    { def = buildDef
    , result = Nothing
    , lastFetch = -1
    , fetching = False
    , fetchError = Nothing
    , previousStatus = Unknown
    }


type alias AddBuildData =
    { bamboo : Bamboo.BambooData
    , travis : Travis.TravisData
    , tab : Int
    , editing : Maybe Build
    }


initialAddBuildData : AddBuildData
initialAddBuildData =
    { bamboo =
        { serverUrl = ""
        , username = ""
        , password = ""
        , plan = ""
        }
    , travis =
        { serverUrl = ""
        , token = ""
        , repository = ""
        , branch = ""
        }
    , tab = 0
    , editing = Nothing
    }


editBuildData : Build -> AddBuildData
editBuildData build =
    let
        bd =
            initialAddBuildData
    in
        { bd
            | bamboo =
                case build.def of
                    BambooDef d ->
                        d
                    _ ->
                        bd.bamboo
            , travis =
                case build.def of
                    TravisDef d ->
                        d
                    _ ->
                        bd.travis
            , editing =
                Just build
            , tab =
                case build.def of
                    BambooDef _ -> 0
                    TravisDef _ -> 1
        }


type View
    = BuildListView
    | AddBuildView


type alias Model =
    { flags : Flags
    , view : View
    , builds : List Build
    , addBuildData : AddBuildData
    , time : Time
    , mdl : Material.Model
    , loaded : Bool
    , loadError : Maybe String
    , snackbar : Snackbar.Model Int
    , dataFileNotFound : Bool
    }


initialModel : Flags -> Model
initialModel flags =
    { flags = flags
    , view = BuildListView
    , builds = []
    , addBuildData = initialAddBuildData
    , time = -1
    , mdl = Material.model
    , loaded = False
    , loadError = Nothing
    , snackbar = Snackbar.model
    , dataFileNotFound = False
    }


type alias PersistedData =
    { bamboo : List Bamboo.BambooData
    , travis : List Travis.TravisData
    }


persistedDataDecoder : Decoder PersistedData
persistedDataDecoder =
    map2 PersistedData
        (field "bamboo" (list Bamboo.bambooDataDecoder))
        (field "travis" (list Travis.travisDataDecoder))


encodePersistedData : PersistedData -> Value
encodePersistedData v =
    JE.object
        [ ( "bamboo"
          , (JE.list
                ( List.map
                    Bamboo.encodeBambooData
                    v.bamboo
                )
            )
          )
        , ( "travis"
          , (JE.list
                (List.map
                    Travis.encodeTravisData
                    v.travis
                )
            )
          )
        ]


getBuildName : BuildDef -> String
getBuildName buildDef =
    case buildDef of
        BambooDef d ->
            d.plan
        TravisDef d ->
            d.repository ++ "/" ++ d.branch


createPersistedData : List Build -> PersistedData
createPersistedData builds =
    { bamboo =
        builds
            |> List.filter (\b ->
                case b.def of
                    BambooDef d -> True
                    _ -> False
            )
            |> List.map (\b ->
                case b.def of
                    BambooDef d -> d
                    _ -> Debug.crash "damnit"
            )
    , travis =
        builds
            |> List.filter (\b ->
                case b.def of
                    TravisDef d -> True
                    _ -> False
            )
            |> List.map (\b ->
                case b.def of
                    TravisDef d -> d
                    _ -> Debug.crash "damnit"
            )
    }
