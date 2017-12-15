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
import Dict


type alias Flags =
    { appName : String
    , appVersion : String
    , dataFileName : String
    }

type alias Tags = List String


type alias BuildId = Int

type alias CommonBuildData =
    { id : BuildId
    , tags : Tags
    }


defaultCommonBuildData : Int -> CommonBuildData
defaultCommonBuildData id =
    { id = id
    , tags = []
    }


type BuildDef
    = BambooDef CommonBuildData BambooData
    | TravisDef CommonBuildData TravisData


getCommonBuildData : BuildDef -> CommonBuildData
getCommonBuildData buildDef =
    case buildDef of
        BambooDef cd _ -> cd
        TravisDef cd _ -> cd


type alias Build =
    { def : BuildDef
    , result : Maybe BuildResult
    , lastFetch : Time
    , fetching : Bool
    , fetchError : Maybe Http.Error
    , previousStatus : Status
    , filtered : Bool
    , hover : Bool
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
    , hover = False
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
    | TagsDialog BuildId String


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
    , layoutTab : Int
    , tagsData : List TagsListItem
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
    , layoutTab = 0
    , tagsData = []
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
    = PersistedBambooBuild Tags Bamboo.BambooData
    | PersistedTravisBuild Tags Travis.TravisData


type alias PersistedData =
    { builds : List PersistedBuild
    , preferences : Preferences
    }


persistedBuildDecoder : Decoder PersistedBuild
persistedBuildDecoder =
    (field "kind" string)
        |> andThen (\k ->
            ( oneOf
                [ field "tags" (list string)
                , succeed []
                ]
            )
                |> andThen (\tags ->
                    case k of
                        "bamboo" -> map (PersistedBambooBuild tags) Bamboo.bambooDataDecoder
                        "travis" -> map (PersistedTravisBuild tags) Travis.travisDataDecoder
                        _ -> fail <| "unsupported kind " ++ k
                )
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


encodeTags : Tags -> (String, Value)
encodeTags tags =
    ("tags", JE.list (List.map JE.string tags))


encodePersistedData : PersistedData -> Value
encodePersistedData v =
    let

        pbToValue pb =
            JE.object <|
                case pb of
                    PersistedBambooBuild tags d ->
                        (Bamboo.encodeBambooData True d)
                            ++ [ encodeTags tags ]

                    PersistedTravisBuild tags d ->
                        (Travis.encodeTravisData True d)
                            ++ [ encodeTags tags ]

        builds =
            v.builds
                |> List.map pbToValue
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
        BambooDef cd _ -> cd.id
        TravisDef cd _ -> cd.id


getBuildById : Model -> Int -> Maybe Build
getBuildById model id =
    model.builds
        |> List.filter (\b -> getDefId b.def == id)
        |> List.head


createPersistedData : Preferences -> List Build -> PersistedData
createPersistedData prefs builds =
    { builds =
        builds
            |> List.map (\b ->
                case b.def of
                    BambooDef cd d ->
                        PersistedBambooBuild cd.tags d
                    TravisDef cd d ->
                        PersistedTravisBuild cd.tags d
            )
    , preferences = prefs
    }


type alias TagsListItem =
    { tag : String
    , nbGreen : Int
    , nbRed : Int
    , raised : Bool
    }


computeTagsDataIfNeeded : Model -> Model
computeTagsDataIfNeeded model =
    if model.layoutTab == 1 then
        { model
            | tagsData =
                computeTagListItems model.builds
        }
    else
        model


computeTagListItems : List Build -> List TagsListItem
computeTagListItems builds =
    let
        -- create a (tag,build) list from a list of builds
        zipTagAndBuild builds tagsAndBuilds =
            case builds of
                b :: bs ->
                    tagsAndBuilds
                        |> List.append
                            ( getCommonBuildData b.def
                                |> .tags
                                |> List.map (\tag ->
                                    (tag, b)
                                )
                            )
                        |> zipTagAndBuild bs
                _ ->
                    tagsAndBuilds

        -- create a dict of (tag -> taglistitem) build
        createDict tagsAndBuilds d =
            case tagsAndBuilds of
                tab :: tabs ->
                    let
                        tag =
                            Tuple.first tab
                        build =
                            Tuple.second tab
                        tli =
                            Dict.get tag d
                                |> Maybe.withDefault
                                    { tag = tag
                                    , nbGreen = 0
                                    , nbRed = 0
                                    , raised = False
                                    }
                        isGreen =
                            build.result
                                |> Maybe.map (\r -> r.status == Green)
                                |> Maybe.withDefault False

                        newTli =
                            { tli
                                | nbGreen = tli.nbGreen +
                                    if isGreen then 1 else 0
                                , nbRed = tli.nbRed +
                                    ( if isGreen then 0 else 1 )
                            }

                        newDict =
                            Dict.insert tag newTli d
                    in
                        createDict tabs newDict
                _ ->
                    d
    in
        createDict
            ( zipTagAndBuild builds [] )
            Dict.empty
                |> Dict.toList
                |> List.map Tuple.second
                |> List.sortBy .tag
