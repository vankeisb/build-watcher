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


type DialogKind
    = AboutDialog
    | PreferencesDialog
    | FetchErrorDialog Build
    

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


type alias PersistedData =
    { bamboo : List Bamboo.BambooData
    , travis : List Travis.TravisData
    , preferences : Preferences
    }


persistedDataDecoder : Decoder PersistedData
persistedDataDecoder =
    map3 PersistedData
        (field "bamboo" (list Bamboo.bambooDataDecoder))
        (field "travis" (list Travis.travisDataDecoder))
        ( oneOf
            [ (field "preferences" preferencesDecoder)
            , succeed initialPreferences
            ]
        )


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
        , ( "preferences", encodePreferences v.preferences )
        ]


getBuildName : BuildDef -> String
getBuildName buildDef =
    case buildDef of
        BambooDef d ->
            d.plan
        TravisDef d ->
            d.repository ++ "/" ++ d.branch


createPersistedData : Preferences -> List Build -> PersistedData
createPersistedData prefs builds =
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
    , preferences = prefs
    }
