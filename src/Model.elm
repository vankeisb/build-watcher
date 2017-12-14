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
    , dataFileName : String
    }

type BuildDef
    = BambooDef Int BambooData
    | TravisDef Int TravisData


type alias Build =
    { def : BuildDef
    , result : Maybe BuildResult
    , lastFetch : Time
    , fetching : Bool
    , fetchError : Maybe Http.Error
    , previousStatus : Status
    , filtered : Bool
    }


defaultBuild : BuildDef -> Build
defaultBuild buildDef =
    { def = buildDef
    , result = Nothing
    , lastFetch = -1
    , fetching = False
    , fetchError = Nothing
    , previousStatus = Unknown
    , filtered = False
    }


type alias AddBuildData =
    { bamboo : Bamboo.BambooData
    , travis : Travis.TravisData
    , tab : Int
    , editing : Maybe Build
    , bambooErrors : Bamboo.BambooValidationErrors
    , travisErrors : Travis.TravisValidationErrors
    , importText : String
    , importError : Maybe String
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
        , travisToken = Nothing
        }
    , tab = 0
    , editing = Nothing
    , bambooErrors =
        { serverUrl = Nothing
        , plan = Nothing
        }
    , travisErrors =
        { serverUrl = Nothing
        , repository = Nothing
        , branch = Nothing
        }
    , importText = ""
    , importError = Nothing
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
                    BambooDef i d ->
                        d
                    _ ->
                        bd.bamboo
            , travis =
                case build.def of
                    TravisDef i d ->
                        d
                    _ ->
                        bd.travis
            , editing =
                Just build
            , tab =
                case build.def of
                    BambooDef _ _ -> 0
                    TravisDef _ _ -> 1
        }


type View
    = BuildListView
    | AddBuildView


type DialogKind
    = AboutDialog
    | PreferencesDialog
    | FetchErrorDialog Build
    | ShareBuildDialog (List Build)


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
    , dialogKind : DialogKind
    , preferences : Preferences
    , counter : Int
    , filterText : String
    , filterVisible : Bool
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
    , dialogKind = AboutDialog
    , preferences = initialPreferences
    , counter = 0
    , filterText = ""
    , filterVisible = False
    }


type alias Preferences =
    { enableNotifications : Bool
    , pollingInterval : Int
    }

initialPreferences : Preferences
initialPreferences =
    { enableNotifications = True
    , pollingInterval = 30
    }


type PersistedBuild
    = PersistedBambooBuild Bamboo.BambooData
    | PersistedTravisBuild Travis.TravisData


type alias PersistedData =
    { builds : List PersistedBuild
    , preferences : Preferences
    }


persistedBuildDecoder : Decoder PersistedBuild
persistedBuildDecoder =
    (field "kind" string)
        |> andThen (\k ->
            case k of
                "bamboo" -> map PersistedBambooBuild Bamboo.bambooDataDecoder
                "travis" -> map PersistedTravisBuild Travis.travisDataDecoder
                _ -> fail <| "unsupported kind " ++ k
        )


persistedDataDecoder : Decoder PersistedData
persistedDataDecoder =
    let
        builds =
            (field "builds" (list persistedBuildDecoder))
        preferences =
            ( oneOf
                [ (field "preferences" preferencesDecoder)
                , succeed initialPreferences
                ]
            )
    in
        map2 PersistedData builds preferences


preferencesDecoder : Decoder Preferences
preferencesDecoder =
    map2 Preferences
        (field "enableNotifications" bool)
        (field "pollingInterval" int)


encodePreferences : Preferences -> Value
encodePreferences v =
    JE.object
        [ ( "enableNotifications", JE.bool v.enableNotifications )
        ]

encodePersistedData : PersistedData -> Value
encodePersistedData v =
    let
        builds =
            v.builds
                |> List.map (\pb ->
                    case pb of
                        PersistedBambooBuild d ->
                            Bamboo.encodeBambooData True d
                        PersistedTravisBuild d ->
                            Travis.encodeTravisData True d
                )
                |> JE.list
    in
        JE.object
            [ ( "builds", builds )
            , ( "preferences", encodePreferences v.preferences )
            ]


getBuildName : BuildDef -> String
getBuildName buildDef =
    case buildDef of
        BambooDef _ d ->
            d.plan
        TravisDef _ d ->
            d.repository ++ "/" ++ d.branch


getDefId : BuildDef -> Int
getDefId buildDef =
    case buildDef of
        BambooDef i _ -> i
        TravisDef i _ -> i


createPersistedData : Preferences -> List Build -> PersistedData
createPersistedData prefs builds =
    { builds =
        builds
            |> List.map (\b ->
                case b.def of
                    BambooDef i d ->
                        PersistedBambooBuild d
                    TravisDef i d ->
                        PersistedTravisBuild d
            )
    , preferences = prefs
    }
