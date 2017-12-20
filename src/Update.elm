module Update exposing (..)

import Bamboo
import Common exposing (Status(Green), Status(Red), Status(Unknown), validateRequired)
import Dom exposing (focus)
import Json.Decode as Json
import Material
import Material.Helpers exposing (map1st, map2nd)
import Material.Snackbar as Snackbar
import Messages exposing (..)
import Model exposing (..)
import Ports
import Task exposing (Task)
import Time exposing (Time)
import Travis


init : Flags -> (Model, Cmd Msg)
init flags =
    ( initialModel (Debug.log "flags" flags)
    , Ports.loadData ()
    )


noCmd : Model -> (Model, Cmd Msg)
noCmd model =
    ( model, Cmd.none )


addToast : String -> Model -> (Model, Cmd Msg)
addToast s model =
    let
        content =
            Snackbar.toast 0 s
        (m, c) =
          Snackbar.add content model.snackbar
            |> map2nd (Cmd.map Snackbar)
        model_ =
          { model
            | snackbar = m
          }
      in
        ( model_
        , c
        )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        OnDataLoadError e ->
            { model
                | loadError = Just <| toString e
                , loaded = True
            }
            |> noCmd

        OnDataLoadFileNotFound fnf ->
            { model
                | loaded = True
                , dataFileNotFound = True
            }
            |> noCmd

        OnDataLoadSuccess persistedDataStr ->
            let
                persistedDataRes =
                    Json.decodeString persistedDataDecoder persistedDataStr
            in
                case persistedDataRes of
                    Ok persistedData ->
                        let
                            builds =
                                persistedData.builds
                                    |> List.indexedMap (\i pb ->
                                        case pb of
                                            PersistedBambooBuild tags d ->
                                                BambooDef
                                                    { id = i, tags = tags }
                                                    d
                                            PersistedTravisBuild tags d ->
                                                TravisDef
                                                    { id = i, tags = tags }
                                                    d
                                    )
                                    |> List.map defaultBuild
                                    |> List.sortBy (\b ->
                                        getBuildName b.def
                                    )
                        in
                            (
                                { model
                                    | builds = builds
                                    , loaded = True
                                    , preferences = persistedData.preferences
                                    , counter = List.length builds
                                }
                                |> applyFilter
                                |> computeTagsDataIfNeeded
                            , fetchNow
                            )
                    Err e ->
                        { model
                            | loaded = True
                            , loadError = Just e
                        }
                        |> noCmd


        OnDataSaveError e ->
            -- TODO
            model ! []

        OnDataSaveSuccess _ ->
            -- TODO
            model ! []

        BuildsViewMsg bvm ->
            updateBuildsView bvm model

        AddBuildViewMsg abvm ->
            updateAddBuildView abvm model

        Tick time ->
            handleTick model time

        FetchResult def res ->
            let
                mapper: Build -> Build
                mapper b =
                    if getDefId b.def == getDefId def then
                        { b
                            | fetching = False
                            , def =
                                case res of
                                    Ok (buildResult,buildDef) ->
                                        buildDef
                                    Err _ ->
                                        b.def
                            , fetchError =
                                case res of
                                    Ok _ ->
                                        Nothing
                                    Err e ->
                                        Just e
                            , result =
                                case res of
                                    Ok (buildResult,buildDef) ->
                                        Just buildResult
                                    Err _ ->
                                        b.result
                            , lastFetch = model.time
                            , previousStatus =
                                -- store only red or green in the previous status
                                case b.result of
                                    Just prevResult ->
                                        case prevResult.status of
                                            Green -> Green
                                            Red -> Red
                                            _ -> prevResult.status
                                    Nothing ->
                                        b.previousStatus
                        }
                    else
                        b

                newBuilds =
                    List.map mapper model.builds
            in
                (
                    { model
                        | builds = newBuilds
                    }
                        |> computeTagsDataIfNeeded
                , newBuilds
                    |> List.map (desktopNotifIfBuildStateChanged model)
                    |> Cmd.batch
                )

        OpenUrl u ->
            ( model
            , Ports.openURL u
            )

        CopyToClipboard s ->
            ( model
            , Ports.copyToClipboard
                { nodeId = "export-data"
                , data = s
                }
            )

        OnCopiedToClipboard nodeId ->
            addToast "Copied to clipboard" model

        LayoutTabClicked i ->
            case model.view of
                BuildListView ->
                    (
                        { model
                            | layoutTab = i
                        }
                            |> computeTagsDataIfNeeded
                    , Cmd.none
                    )

                AddBuildView ->
                    model ! []


        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


fetch : Build -> Time -> (Build, Cmd Msg)
fetch build time =
    (
        { build
            | lastFetch = time
            , fetching = True
        }
    ,
        ( case build.def of
            BambooDef i bambooData ->
                Bamboo.fetch bambooData
                    |> Task.map (\br -> (br, BambooDef i bambooData))
            TravisDef i travisData ->
                Travis.fetch travisData
                    |> Task.map (\(br, td) -> (br, TravisDef i td))
        )
        |> Task.attempt (FetchResult build.def)
    )


fetchNow : Cmd Msg
fetchNow =
    Time.now
        |> Task.perform (\now -> BuildsViewMsg <| BVNowReceived now)


handleTick : Model -> Time -> (Model, Cmd Msg)
handleTick model time =
    let
        newBuilds =
            model.builds
                |> List.map (\build ->
                    if time - build.lastFetch > 5 * Time.second && not build.fetching then
                        -- outdated : fetch
                        fetch build time

                    else
                        -- no need to fetch
                        ( build, Cmd.none )
                )
    in
        (
            { model
                | builds = List.map Tuple.first newBuilds
                , time = time
            }
        , newBuilds
            |> List.map Tuple.second
            |> Cmd.batch
        )


updateAddBuildView : ABVMsg -> Model -> (Model, Cmd Msg)
updateAddBuildView abvm model =
    case abvm of

        ABTabClicked t ->
            let
                abd =
                    model.addBuildData
                newAbd =
                    { abd
                        | tab = t
                    }
            in
                { model
                    | addBuildData = newAbd
                }
                |> noCmd

        ABCancelClicked ->
            { model
                | view = BuildListView
                , addBuildData = initialAddBuildData
            }
            |> noCmd

        ABOkClicked ->
            let
                sanitize buildData =
                    { buildData
                        | serverUrl =
                            if String.endsWith "/" buildData.serverUrl then
                                String.dropRight 1 buildData.serverUrl
                            else
                                buildData.serverUrl
                    }

                abd =
                    model.addBuildData

                newCounter =
                    model.counter + 1

                (newDefs, importError) =
                    case abd.tab of
                        0 ->
                            (
                                [ BambooDef
                                    (defaultCommonBuildData newCounter)
                                    (sanitize abd.bamboo)
                                ]
                            , Nothing
                            )
                        1 ->
                            (
                                [ TravisDef
                                    (defaultCommonBuildData newCounter)
                                    (sanitize abd.travis)
                                ]
                            , Nothing
                            )
                        2 ->
                            let
                                decoded =
                                    abd.importText
                                        |> Json.decodeString (Json.list persistedBuildDecoder)
                                parsedBuildDefs =
                                    decoded
                                        |> Result.map (
                                            List.indexedMap (\i pb ->
                                                case pb of
                                                    PersistedBambooBuild tags d ->
                                                        BambooDef { id = i + newCounter, tags = tags } d
                                                    PersistedTravisBuild tags d ->
                                                        TravisDef { id = i + newCounter, tags = tags } d
                                            )
                                        )
                                        |> Result.withDefault []
                                error =
                                    case decoded of
                                        Ok _ -> Nothing
                                        Err e -> Just e
                            in
                                ( parsedBuildDefs, error )
                        _ ->
                            Debug.crash "invalid tab"

                newBuilds =
                    ( case abd.editing of
                        Just build ->
                            model.builds
                                |> List.map (\b ->
                                    if getDefId b.def == getDefId build.def then
                                        newDefs
                                            |> List.head
                                            |> Maybe.map defaultBuild
                                            |> Maybe.withDefault b
                                    else
                                        b
                                )
                        Nothing ->
                            model.builds ++
                                ( newDefs
                                    |> List.map defaultBuild
                                )
                    )
                    |> List.sortBy (\b ->
                        getBuildName b.def
                    )

                newModel =
                    { model
                        | counter = model.counter + (List.length newDefs)
                        , addBuildData =
                            { abd
                                | importError = importError
                            }
                    }
                    |> applyFilter
            in
                case importError of
                    Just error ->
                        newModel
                            |> noCmd
                    Nothing ->
                        updateBuildsAndSave
                            newModel
                            newBuilds
                            ( case abd.editing of
                                Just editedBuild ->
                                    "Build " ++ (getBuildName editedBuild.def) ++ " updated"
                                Nothing ->
                                    if List.length newDefs == 1 then
                                        "Build added"
                                    else
                                        (toString <| List.length newDefs) ++ " build(s) added"
                            )

        ABBambooServerUrlChanged s ->
            noCmd <|
                mapBambooBuildData
                    model
                    (\(b, be) ->
                        ( { b | serverUrl = s }
                        , { be | serverUrl = validateRequired s }
                        )
                    )

        ABBambooUsernameChanged s ->
            noCmd <|
                mapBambooBuildData
                    model
                    (\(b, be) ->
                        ( { b | username = s }
                        , be
                        )
                    )

        ABBambooPasswordChanged s ->
            noCmd <|
                mapBambooBuildData
                    model
                    (\(b, be) ->
                        ( { b | password = s }
                        , be
                        )
                    )

        ABBambooPlanChanged s ->
            noCmd <|
                mapBambooBuildData
                    model
                    (\(b, be) ->
                        ( { b | plan = s }
                        , { be | plan = validateRequired s }
                        )
                    )

        ABTravisServerUrlChanged s ->
            noCmd <|
                mapTravisBuildData
                    model
                    (\(t, te) ->
                        ( { t | serverUrl = s }
                        , { te | serverUrl = validateRequired s }
                        )
                    )

        ABTravisTokenChanged s ->
            noCmd <|
                mapTravisBuildData
                    model
                    (\(t, te) ->
                        ( { t | token = s }
                        , te
                        )
                    )

        ABTravisRepoChanged s ->
            noCmd <|
                mapTravisBuildData
                    model
                    (\(t, te) ->
                        ( { t | repository = s }
                        , { te | repository = validateRequired s }
                        )
                    )

        ABTravisBranchChanged s ->
            noCmd <|
                mapTravisBuildData
                    model
                    (\(t, te) ->
                        ( { t | branch = s }
                        , { te | branch = validateRequired s }
                        )
                    )

        ABImportTextChanged s ->
            let
                abd =
                    model.addBuildData
                newAbd =
                    { abd
                        | importText = s
                        , importError = Nothing
                    }
            in
                noCmd <|
                    { model
                        | addBuildData = newAbd
                    }


updateBuildsView : BVMsg -> Model -> (Model, Cmd Msg)
updateBuildsView bvm model =
    case bvm of
        BVNowReceived now ->
            handleTick model now

        BVAddBuildClicked ->
            noCmd <|
                { model
                    | addBuildData = initialAddBuildData
                    , view = AddBuildView
                }

        BVEditClicked build ->
            noCmd <|
                { model
                    | addBuildData = editBuildData build
                    , view = AddBuildView
                }

        BVDeleteClicked build ->
            let
                (m, c) =
                    addToast
                        ( (getBuildName build.def) ++ " deleted" )
                        model

                newBuilds =
                    model.builds
                        |> List.filter (\b -> b.def /= build.def)

                newPersistedData =
                    createPersistedData model.preferences newBuilds
            in
                (
                    { m
                        | builds = newBuilds
                    }
                , Cmd.batch
                    [ save newPersistedData
                    , c
                    ]
                )

        BVCopyClicked build ->
            let
                newCounter =
                    model.counter + 1

                newCd cd =
                    { cd | id = newCounter }

                newDef =
                    case build.def of
                        BambooDef cd d -> BambooDef (newCd cd) (Bamboo.copy d)
                        TravisDef cd d -> TravisDef (newCd cd) (Travis.copy d)

                newBuilds =
                    model.builds
                        |> List.append [ defaultBuild newDef ]
                        |> List.sortBy (\b ->
                            getBuildName b.def
                        )
            in
                updateBuildsAndSave
                    { model | counter = newCounter }
                    newBuilds
                    ( (getBuildName newDef) ++ " duplicated" )


        BVAboutClicked ->
            showDialog model AboutDialog

        BVPrefsClicked ->
            showDialog model PreferencesDialog

        BVShareClicked build ->
            showDialog model <| ShareBuildDialog [ build ]

        BVTagsClicked build ->
            showDialog model <| TagsDialog (getDefId build.def) ""

        BVPrefsToggleNotif ->
            updatePrefsAndSave
                model
                (\p ->
                    { p
                        | enableNotifications =
                            not p.enableNotifications
                    }
                )

        BVPrefsPollingChanged s ->
            updatePrefsAndSave
                model
                (\p ->
                    { p
                        | pollingInterval =
                            String.toInt s
                                |> Result.withDefault 0
                    }
                )

        BVBuildClicked b ->
            case b.result of
                Just result ->
                    ( model
                    , Ports.openURL result.url
                    )
                Nothing ->
                    case b.fetchError of
                        Just err ->
                            showDialog model <| FetchErrorDialog b
                        Nothing ->
                            (model, Cmd.none)

        BVCloseClicked ->
            ( model, Ports.closeWindow () )

        BVFilterChanged s ->
            doFilter model s

        BVClearFilter ->
            doFilter
                { model
                    | filterVisible = False
                }
                ""

        BVShareAllClicked ->
            showDialog model <| ShareBuildDialog model.builds

        BVShowFilterClicked ->
            ( { model | filterVisible = True }
            , focus "filter-box"
                |> Task.attempt (\r -> BuildsViewMsg BVFilterFocusResult)
            )

        BVFilterFocusResult ->
            -- do nothing !
            model ! []

        BVDeleteTagClicked tag ->
            withTagsDialogStuff model (\build tagsText ->
                updateTags
                    model
                    build
                    ( getCommonBuildData build.def
                        |> .tags
                        |> List.filter (\t -> t /= tag)
                    )
            )

        BVTagsChanged s ->
            withTagsDialogStuff model (\build tagsText ->
                showDialog model <| TagsDialog (getDefId build.def) s
            )

        BVTagClicked tag ->
            showDialog model
                <| TagDetailsDialog (computeTagDetailsData model tag)

        BVTagsKeyUp keyCode ->
            if keyCode == 13 then
                -- ENTER : create tag(s)
                withTagsDialogStuff model (\build tagsText ->
                    updateTags
                        { model
                            | dialogKind =
                                TagsDialog
                                    (getDefId build.def)
                                    ""
                        }
                        build
                        (
                            ( getCommonBuildData build.def
                                |> .tags
                            ) ++
                            ( tagsText
                                |> String.trim
                                |> String.split " "
                            )
                        )
                )
            else
                model ! []

        BVRaiseTag tag ->
            { model
                | tagsData =
                    model.tagsData
                        |> List.map (\td ->
                            { td | raised = td.tag == tag }
                        )
            }
                |> noCmd

        BVBuildHover hover build ->
            { model
                | builds =
                    model.builds
                        |> List.map (\b ->
                            { b
                                | hover =
                                    getDefId b.def == getDefId build.def
                            }
                        )
            }
                |> noCmd


showDialog : Model -> DialogKind -> (Model, Cmd Msg)
showDialog model dialogKind =
    noCmd { model | dialogKind = dialogKind }


updateTags : Model -> Build -> Tags -> (Model, Cmd Msg)
updateTags model build newTags =
    let
        newBuilds =
            model.builds
                |> List.map (\b ->
                    if getDefId b.def == getDefId build.def then
                        { b
                            | def =
                                case b.def of
                                    BambooDef cd d ->
                                        BambooDef { cd | tags = newTags } d
                                    TravisDef cd d ->
                                        TravisDef { cd | tags = newTags } d
                        }
                    else
                        b
                )
    in
        updateBuildsAndSave
            model
            newBuilds
            ""



withTagsDialogStuff : Model -> (Build -> String -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
withTagsDialogStuff model f =
    case model.dialogKind of
        TagsDialog buildId tagsText ->
            case getBuildById model buildId of
                Just build ->
                    f build tagsText
                _ ->
                    model ! []
        _ ->
            model ! []


applyFilter : Model -> Model
applyFilter model =
    { model
        | builds =
            model.builds
                |> List.map (\b ->
                    { b
                        | filtered =
                            if String.isEmpty model.filterText then
                                False
                            else
                                not <|
                                    String.contains
                                        (String.toLower model.filterText)
                                        (String.toLower <|
                                            (getBuildName b.def) ++
                                                (getCommonBuildData b.def
                                                    |> .tags
                                                    |> String.join " "
                                                )
                                        )
                    }
                )
    }


doFilter : Model -> String -> (Model, Cmd Msg)
doFilter model newFilter =
    ( applyFilter { model | filterText = newFilter }
    , Cmd.none
    )


updateBuildsAndSave : Model -> List Build -> String -> (Model, Cmd Msg)
updateBuildsAndSave model newBuilds toastText =
    let
        newPersistedData =
            createPersistedData model.preferences newBuilds
        (m, c) =
            if String.isEmpty toastText then
                (model, Cmd.none)
            else
                addToast toastText model
    in
        (
            { m
                | builds = newBuilds
                , view = BuildListView
            }
                |> computeTagsDataIfNeeded
        , Cmd.batch
            [ save newPersistedData
            , c
            , fetchNow
            ]
        )


updatePrefsAndSave : Model -> (Preferences -> Preferences) -> (Model, Cmd Msg)
updatePrefsAndSave model f =
    let
        newPrefs =
            f model.preferences
    in
        (
            { model
                | preferences = newPrefs
            }
        , createPersistedData newPrefs model.builds
            |> save
        )


mapBambooBuildData : Model -> ((Bamboo.BambooData, Bamboo.BambooValidationErrors) -> (Bamboo.BambooData, Bamboo.BambooValidationErrors)) -> Model
mapBambooBuildData model f =
    let
        abd =
            model.addBuildData

        (bamboo, bambooErrors) =
            f (abd.bamboo, abd.bambooErrors)
    in
        { model
            | addBuildData =
                { abd
                    | bamboo = bamboo
                    , bambooErrors = bambooErrors
                }
        }


mapTravisBuildData : Model -> ((Travis.TravisData, Travis.TravisValidationErrors) -> (Travis.TravisData, Travis.TravisValidationErrors)) -> Model
mapTravisBuildData model f =
    let
        abd =
            model.addBuildData
        (travis, travisErrors) =
            f (abd.travis, abd.travisErrors)
    in
        { model
            | addBuildData =
                { abd
                    | travis = travis
                    , travisErrors = travisErrors
                }
        }


desktopNotifIfBuildStateChanged : Model -> Build -> Cmd m
desktopNotifIfBuildStateChanged model build =
    if model.preferences.enableNotifications then
        case build.result of
            Just result ->
                if build.previousStatus /= Unknown
                    && build.previousStatus /= result.status then
                    { title = result.name
                    , body =
                        case result.status of
                            Green -> "Build is now green"
                            Red -> "Build has failed"
                            _ -> ""
                    , isGreen =
                        result.status == Green
                    }
                    |> Ports.desktopNotification
                else
                    Cmd.none
            Nothing ->
                Cmd.none
    else
        Cmd.none


save : PersistedData -> Cmd m
save p =
    encodePersistedData p
        |> Ports.saveData


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ case model.view of
            BuildListView ->
                if model.preferences.pollingInterval > 0 then
                    Time.every (Time.second * (toFloat model.preferences.pollingInterval)) Tick
                else
                    Sub.none
            AddBuildView ->
                Sub.none
        ] ++
        [ Material.subscriptions Mdl model
        , Ports.onDataLoadError OnDataLoadError
        , Ports.onDataLoadFileNotFound OnDataLoadFileNotFound
        , Ports.onDataLoadSuccess OnDataLoadSuccess
        , Ports.onDataSaveError OnDataSaveError
        , Ports.onDataSaveSuccess OnDataSaveSuccess
        , Ports.onCopiedToClipboard OnCopiedToClipboard
        ]
