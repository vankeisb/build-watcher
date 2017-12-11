module Update exposing (..)

import Bamboo
import Common exposing (Status(Green), Status(Red), Status(Unknown))
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

        OnDataLoadSuccess persistedData ->
            let
                bambooBuilds =
                    persistedData.bamboo
                        |> List.map BambooDef

                travisBuilds =
                    persistedData.travis
                        |> List.map TravisDef
            in
                (
                    { model
                        | builds =
                            bambooBuilds
                                |> List.append travisBuilds
                                |> List.map defaultBuild
                                |> List.sortBy (\b ->
                                    getBuildName b.def
                                )
                        , loaded = True
                    }
                , Cmd.none
                )

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
                mapper b =
                    if b.def == def then
                        { b
                            | fetching = False
                            , fetchError =
                                case res of
                                    Ok _ ->
                                        Nothing
                                    Err e ->
                                        Just e
                            , result =
                                case res of
                                    Ok r ->
                                        Just r
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
                , newBuilds
                    |> List.map desktopNotifIfBuildStateChanged
                    |> Cmd.batch
                )

        OpenUrl u ->
            ( model
            , Ports.openURL u
            )


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
            BambooDef bambooData ->
                Bamboo.fetch bambooData
            TravisDef travisData ->
                Travis.fetch travisData
        )
        |> Task.attempt (FetchResult build.def)
    )


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
                (
                    { model
                        | addBuildData = newAbd
                    }
                , Cmd.none

                )

        ABCancelClicked ->
            (
                { model
                    | view = BuildListView
                    , addBuildData = initialAddBuildData
                }
            , Cmd.none
            )

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

                newDef =
                    if abd.tab == 0 then
                        BambooDef (sanitize abd.bamboo)
                    else
                        TravisDef (sanitize abd.travis)

                newBuilds =
                    case abd.editing of
                        Just build ->
                            model.builds
                                |> List.map (\b ->
                                    if b.def == build.def then
                                        defaultBuild newDef
                                    else
                                        b
                                )
                                |> List.sortBy (\b ->
                                    getBuildName b.def
                                )
                        Nothing ->
                            model.builds ++
                                [ defaultBuild newDef
                                ]

                newPersistedData =
                    createPersistedData newBuilds

                toastMsg =
                    case abd.editing of
                        Just editedBuild ->
                            "Build " ++ (getBuildName newDef) ++ " updated"
                        Nothing ->
                            "Build added : " ++ (getBuildName newDef)
                (m, c) =
                    addToast toastMsg model
            in
                (
                    { m
                        | builds = newBuilds
                        , view = BuildListView
                    }
                , Cmd.batch
                    [ Ports.saveData newPersistedData
                    , c
                    ]
                )

        ABBambooServerUrlChanged s ->
            ( mapBambooBuildData
                model
                (\bbd -> { bbd | serverUrl = s } )
            , Cmd.none
            )

        ABBambooUsernameChanged s ->
            ( mapBambooBuildData
                model
                (\bbd -> { bbd | username = s } )
            , Cmd.none
            )

        ABBambooPasswordChanged s ->
            ( mapBambooBuildData
                model
                (\bbd -> { bbd | password = s } )
            , Cmd.none
            )

        ABBambooPlanChanged s ->
            ( mapBambooBuildData
                model
                (\bbd -> { bbd | plan = s } )
            , Cmd.none
            )

        ABTravisServerUrlChanged s ->
            ( mapTravisBuildData
                model
                (\d -> { d | serverUrl = s } )
            , Cmd.none
            )

        ABTravisTokenChanged s ->
            ( mapTravisBuildData
                model
                (\d -> { d | token = s } )
            , Cmd.none
            )

        ABTravisRepoChanged s ->
            ( mapTravisBuildData
                model
                (\d -> { d | repository = s } )
            , Cmd.none
            )

        ABTravisBranchChanged s ->
            ( mapTravisBuildData
                model
                (\d -> { d | branch = s } )
            , Cmd.none
            )

updateBuildsView : BVMsg -> Model -> (Model, Cmd Msg)
updateBuildsView bvm model =
    case bvm of
        BVAddBuildClicked ->
            (
                { model
                    | addBuildData = initialAddBuildData
                    , view = AddBuildView
                }
            , Cmd.none
            )

        BVEditClicked build ->
            (
                { model
                    | addBuildData = editBuildData build
                    , view = AddBuildView
                }
            , Cmd.none
            )

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
                    createPersistedData newBuilds
            in
                (
                    { m
                        | builds = newBuilds
                    }
                , Cmd.batch
                    [ Ports.saveData newPersistedData
                    , c
                    ]
                )

mapBambooBuildData : Model -> (Bamboo.BambooData -> Bamboo.BambooData) -> Model
mapBambooBuildData model f =
    let
        abd =
            model.addBuildData
        newAbd =
            { abd | bamboo = f abd.bamboo }
    in
        { model
            | addBuildData = newAbd
        }


mapTravisBuildData : Model -> (Travis.TravisData -> Travis.TravisData) -> Model
mapTravisBuildData model f =
    let
        abd =
            model.addBuildData
        newAbd =
            { abd | travis = f abd.travis }
    in
        { model
            | addBuildData = newAbd
        }


desktopNotifIfBuildStateChanged : Build -> Cmd m
desktopNotifIfBuildStateChanged build =
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ case model.view of
            BuildListView ->
                Time.every (Time.second * 5) Tick
            AddBuildView ->
                Sub.none
        ] ++
        [ Material.subscriptions Mdl model
        , Ports.onDataLoadError OnDataLoadError
        , Ports.onDataLoadFileNotFound OnDataLoadFileNotFound
        , Ports.onDataLoadSuccess OnDataLoadSuccess
        , Ports.onDataSaveError OnDataSaveError
        , Ports.onDataSaveSuccess OnDataSaveSuccess
        ]
