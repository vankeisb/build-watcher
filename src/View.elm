module View exposing (..)


import Bamboo
import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import Json.Encode
import Material.Button as Button
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as Lists
import Material.Menu as Menu
import Material.Options as Options exposing (cs, css)
import Material.Scheme
import Material.Snackbar as Snackbar
import Material.Tabs as Tabs
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import Messages exposing (..)
import Model exposing (..)
import Style exposing (..)
import Travis
import Material.Chip as Chip
import Material.Card as Card


i : String -> Html m
i name =
    Icon.view name [ css "width" "40px" ]

padding: Options.Property c m
padding =
    css "padding-right" "18px"


view : Model -> Html Msg
view model =
    Layout.render Mdl model.mdl
        [ Layout.fixedHeader
        , Layout.fixedTabs
        , Layout.selectedTab model.layoutTab
        , Layout.onSelectTab LayoutTabClicked
        ]
        { header =
            [ Layout.row
                [ css "padding-left" "4px"
                , css "padding-right" "4px"
                ]
                [ Layout.title
                    [ css "align-items" "center"
                    , css "display" "flex"
                    , css "margin-left" "48px"
                    , css "flex-grow" "1"
                    , css "justify-content" "center"
                    , css "-webkit-app-region" "drag"
                    ]
                    [ Icon.view "remove_red_eye"
                        [ css "padding-right" "12px"
                        ]
                    , span []
                        [ case model.view of
                            BuildListView ->
                                text model.flags.appName
                            AddBuildView ->
                                case model.addBuildData.editing of
                                    Just build ->
                                        text <| getBuildName build.def
                                    Nothing ->
                                        text "Add"
                        ]
                    ]
                , case model.view of
                    BuildListView ->
                        div
                            [ style
                                [ displayFlex
                                , alignItemsCenter
                                ]
                            ]
                            [
                                if model.layoutTab /= 1
                                    && not model.filterVisible then
                                    Button.render Mdl [0, 0] model.mdl
                                        [ Button.icon
                                        , Button.ripple
                                        , Options.onClick <|
                                            BuildsViewMsg BVShowFilterClicked
                                        ]
                                        [ Icon.i "search"
                                        ]
                                else
                                    -- fill up the space to avoid the title to move
                                    div [ style [ ("width", "32px")] ] []
                            ,
                                case model.view of
                                    AddBuildView ->
                                        Button.render Mdl [ 0, 0, 0 ] model.mdl
                                            [ Button.icon
                                            , Button.ripple
                                            , Options.onClick
                                                <| AddBuildViewMsg ABCancelClicked
                                            ]
                                            [ Icon.i "close"
                                            ]
                                    _ ->
                                        text ""
                            ]

                    AddBuildView ->
                        Button.render Mdl [0, 2] model.mdl
                            [ Button.icon
                            , Button.ripple
                            , Options.onClick <| AddBuildViewMsg ABCancelClicked
                            ]
                            [ Icon.i "close"
                            ]
                ]
            ]
        , drawer = viewDrawer model
        , tabs =
            (
                case model.view of
                    BuildListView ->
                        [ text "Builds"
                        , text "Tags"
                        ]
                    AddBuildView ->
                        []
            ,
                []
            )
        , main =
            (
            if model.loaded then
                case model.loadError of
                    Just loadError ->
                        [ p
                            []
                            [ text <| "Error loading your data : " ++ loadError
                            ]
                        , p
                            []
                            [ text "Please fix (or delete) the data file at :"]
                        , code
                            []
                            [ text model.flags.dataFileName
                            ]
                        ]

                    Nothing ->
                        case model.view of
                            BuildListView ->
                                viewBuildList model
                            AddBuildView ->
                                viewAddBuild model
            else
                [ p
                    []
                    [ text "Loading..."
                    ]
                ]
            ) ++
                [ dialog model
                , Snackbar.view model.snackbar |> Html.map Snackbar
                ]

        }
        -- |> Material.Scheme.top --WithScheme Color.BlueGrey Color.Blue


viewDrawer : Model -> List (Html Msg)
viewDrawer model =
    let
        navItem icon label click openDialog =
            Layout.link
                [ Options.onClick <| BuildsViewMsg click
                -- , Options.when (not openDialog)
                --     <| Options.onClick (Layout.toggleDrawer Mdl)
                , Options.when openDialog
                    <| Dialog.openOn "click"
                ]
                [ Options.div
                    [ css "display" "flex"
                    , css "align-items" "center"
                    , Options.onClick <|
                        BuildsViewMsg click
                    ]
                    [ i icon
                    , div [] [ text label ]
                    ]
                ]
    in
        case model.view of
            BuildListView ->
                [ Layout.title
                    [ css "display" "flex"
                    , css "align-items" "center"
                    ]
                    [ i "remove_red_eye"
                    , text <| model.flags.appName
                    ]
                , Layout.navigation
                    []
                    [ navItem "add" "Add..." BVAddBuildClicked False
                    , navItem "share" "Share builds" BVShareAllClicked True
                    , navItem "build" "Preferences" BVPrefsClicked True
                    , navItem "help" "About" BVAboutClicked True
                    , navItem "close" "Hide window" BVCloseClicked False
                    ]
                ]

            AddBuildView ->
                []


viewBuildList : Model -> List (Html Msg)
viewBuildList model =
    [
        if List.isEmpty model.builds then
            div
                [ style
                    [ position "absolute"
                    , top0
                    , bottom0
                    , left0
                    , right0
                    , displayFlex
                    ]
                ]
                [ div
                    [ style
                        [ displayFlex
                        , alignItemsCenter
                        , flexGrow
                        ]
                    ]
                    [ Options.styled p
                        [ Typo.center
                        , Typo.subhead
                        , css "width" "100%"
                        ]
                        [ text <|
                            if model.dataFileNotFound then
                                "Welcome to " ++ model.flags.appName ++ " ! Add builds using the main menu."
                            else
                                if String.isEmpty model.filterText then
                                    "No builds are monitored. Add builds using main menu."
                                else
                                    "No builds matching criteria"
                        ]
                    ]
                ]
        else
            div
                [ style
                    [ positionAbsolute
                    , top0
                    , bottom0
                    , right0
                    , left0
                    , ( "overflow", "hidden" )
                    , displayFlex
                    ]
                ]
                [ div
                    [ style
                        [ displayFlex
                        , flexColumn
                        , flexGrow
                        ]
                    ]
                    [ div
                        []
                        [ viewSearchBar model ]
                    , div
                        [ style
                            [ flexGrow
                            , positionRelative
                            ]
                        ]
                        [ div
                            [ style
                                [ positionAbsolute
                                , top0
                                , bottom0
                                , right0
                                , left0
                                , ( "overflow-y", "auto" )
                                ]
                            ]
                            [ case model.layoutTab of
                                0 ->
                                    viewBuilds model
                                1 ->
                                    viewTags model
                                _ ->
                                    Debug.crash "unknown tab"
                            ]
                        ]
                    ]
                ]
    ]

viewBuilds : Model -> Html Msg
viewBuilds model =
    div
        [ style [ displayFlex, flexColumn ] ]
        ( model.builds
                |> List.filter (\b -> not b.filtered)
                |> List.indexedMap
                    (viewDefAndResult model)
        )


viewDefAndResult : Model -> Int -> Build -> Html Msg
viewDefAndResult model index b =
    let
        (avatarIcon, hue) =
            case b.result of
                Just r ->
                    case r.status of
                        Unknown ->
                            ("play_circle_outline", Color.Grey)
                        Building ->
                            -- use prev result to change color
                            -- when the build is in progress
                            ( "autorenew"
                            , case b.previousStatus of
                                Red -> Color.Red
                                Green -> Color.Green
                                _ -> Color.Grey
                            )
                        Green ->
                            ("mood", Color.Green)
                        Red ->
                            ("mood_bad", Color.Red)
                Nothing ->
                    case b.fetchError of
                        Just e ->
                            ("error_outline", Color.DeepPurple)
                        Nothing ->
                            ("play_circle_outline", Color.Grey)

        padding =
            css "padding-right" "18px"

        (browseEnabledAttr, browseSelectAttr) =
            case b.result of
                Just result ->
                    ( Options.nop
                    , Menu.onSelect <| BuildsViewMsg (BVBuildClicked b)
                    )
                Nothing ->
                    ( Menu.disabled
                    , Options.nop
                    )
        tags =
            getCommonBuildData b.def
                |> .tags

        onBuildClicked =
            Html.Events.onClick (BuildsViewMsg (BVBuildClicked b))
    in
        div
            [ style <|
                [ displayFlex
                , alignItemsCenter
                , ("padding-top", "8px")
                , ("padding-bottom", "8px")
                ] ++
                (
                    if b.hover then
                        [ ("background-color", "#EEEEEE")
                        , ("cursor", "pointer")
                        ]
                    else
                        []
                )
            , Html.Events.onMouseEnter <| BuildsViewMsg (BVBuildHover True b)
            , Html.Events.onMouseLeave <| BuildsViewMsg (BVBuildHover False b)
            ]
            [ div
                [ style [ displayFlex, alignItemsCenter ]
                ]
                [ Icon.view avatarIcon
                    [ Color.background (Color.color hue Color.S500)
                    , css "border-radius" "50%"
                    , css "color" "white"
                    , css "margin-left" "8px"
                    , css "margin-right" "8px"
                    , Icon.size48
                    ]
                ]
            , div
                [ style <|
                    [ displayFlex
                    , flexColumn
                    , flexGrow
                    , ("overflow", "hidden")
                    ]
                , onBuildClicked
                ]
                [ Options.div
                    (
                        [ Typo.subhead
                        , css "line-height" "inherit"
                        ] ++ wordWrap
                    )
                    [ text <| getBuildName b.def ]
                ,
                    if List.isEmpty tags then
                        text ""
                    else
                        div [ style [ flexGrow, displayFlex, flexWrap ] ]
                            ( getCommonBuildData b.def
                                |> .tags
                                |> List.map (\tag ->
                                    Chip.span
                                        [ css "margin" "0px 4px 4px 0px"
                                        , css "height" "20px"
                                        , css "line-height" "20px"
                                        , css "padding" "0 6px"
                                        ]
                                        [ Chip.content
                                            [ css "font-size" "10px" ]
                                            [ text tag ]
                                        ]
                                )
                            )
                ]
            ,
                if b.hover then
                    div []
                        [ Menu.render Mdl [ 1, index ] model.mdl
                            [ Menu.bottomRight
                            , Menu.ripple
                            ]
                            [ Menu.item
                                [ browseSelectAttr
                                , browseEnabledAttr
                                , padding
                                ]
                                [ i "launch"
                                , text "Browse"
                                ]
                            , Menu.item
                                [ Menu.onSelect <| BuildsViewMsg (BVEditClicked b)
                                , padding
                                ]
                                [ i "edit"
                                , text "Edit"
                                ]
                            , Menu.item
                                [ Menu.onSelect <| BuildsViewMsg (BVTagsClicked b)
                                , padding
                                , Dialog.openOn "click"
                                ]
                                [ i "label_outline"
                                , text "Tag"
                                ]
                            , Menu.item
                                [ Menu.onSelect <| BuildsViewMsg (BVCopyClicked b)
                                , padding
                                ]
                                [ i "content_copy"
                                , text "Duplicate"
                                ]
                            , Menu.item
                                [ Dialog.openOn "click"
                                , Menu.onSelect <| BuildsViewMsg (BVShareClicked b)
                                , padding
                                , Menu.divider
                                ]
                                [ i "share"
                                , text "Share"
                                ]
                            , Menu.item
                                [ Menu.onSelect <| BuildsViewMsg (BVDeleteClicked b)
                                ]
                                [ i "delete"
                                , text "Delete"
                                ]
                            ]
                        ]
                else
                    div
                        [ style
                            [ ("min-width", "32px")
                            , ("max-width", "32px")
                            ]
                        ]
                        []
            ]

viewTags : Model -> Html Msg
viewTags model =
    div
        [ style
            [ ("display", "grid")
            , ("grid-template-columns", "repeat(auto-fill, minmax(150px, 1fr) ) ")
            ]
        ]
        ( model.tagsData
            |> List.map (\cell ->
                div []
                    [ viewTag cell
                    ]
            )
        )

white : Options.Property c m
white =
  Color.text Color.white


viewTag : TagsListItem -> Html Msg
viewTag tagsListItem =
    div [ style [ ("padding", "8px")] ]
        [ Card.view
            [ Color.background <|
                if tagsListItem.nbRed == 0 then
                    Color.color Color.Green Color.S500
                else
                    Color.color Color.Red Color.S500
            , css "width" "100%"
            , css "overflow" "hidden"
            , if tagsListItem.raised then Elevation.e8 else Elevation.e2
            , Elevation.transition 250
            , Options.onMouseEnter (BuildsViewMsg <| BVRaiseTag tagsListItem.tag)
            , Options.onMouseLeave (BuildsViewMsg <| BVRaiseTag "")
            , Options.onClick (BuildsViewMsg <| BVTagClicked tagsListItem.tag)
            , Dialog.openOn "click"
            ]
            [ Card.title
                [ css "overflow" "hidden"
                , css "display" "block"
                ]
                [ Card.head
                    (white :: (css "display" "block") :: wordWrap)
                    [ text tagsListItem.tag ]
                ]
            , Card.text [ white ]
                [ div
                    [ style
                         [ displayFlex
                         , alignItemsCenter
                         ]
                    ]
                    [ Icon.i "mood"
                    , Options.span
                        [ Typo.headline
                        , css "padding-left" "4px"
                        , css "padding-right" "16px"
                        ]
                        [ text <| toString tagsListItem.nbGreen ]
                    , Icon.i "mood_bad"
                    , Options.span
                        [ Typo.headline
                        , css "padding-left" "8px"
                        ]
                        [ text <| toString tagsListItem.nbRed ]
                    ]
                ]
            ]
        ]

viewSearchBar : Model -> Html Msg
viewSearchBar model =
    Options.div
        [ Elevation.e2
        , css "padding" "8px"
        , css "display" <|
            if model.layoutTab /= 1 && model.filterVisible then
                "block"
            else
                "none"
        ]
        [ div
            [ style
                [ displayFlex
                , alignItemsCenter
                ]
            ]
            [ Textfield.render Mdl [ 100 ] model.mdl
                [ Textfield.label "Filter builds..."
                , Options.css "flex-grow" "1"
                , Options.onInput (\s -> BuildsViewMsg (BVFilterChanged s))
                , Textfield.value model.filterText
                , Options.id "filter-box"
                ]
                []
            , div
                [ style
                    [ displayFlex
                    , alignItemsCenter
                    ]
                ]
                [ Button.render Mdl [ 102 ] model.mdl
                    [ Button.ripple
                    , Button.icon
                    , Options.onClick <| BuildsViewMsg BVClearFilter
                    ]
                    [ Icon.i "close"
                    ]
                ]
            ]
        ]


viewAddTabs : Model -> Html Msg
viewAddTabs model =
    case model.addBuildData.editing of
        Just build ->
            text ""
        Nothing ->
            Tabs.render Mdl [3] model.mdl
                [ Tabs.ripple
                , Tabs.activeTab model.addBuildData.tab
                ,
                    case model.addBuildData.editing of
                        Just build ->
                            Options.nop
                        Nothing ->
                            Tabs.onSelectTab (\t -> AddBuildViewMsg (ABTabClicked t))
                ]
                [ Tabs.label
                    [ Options.center
                    ]
                    [ text "Bamboo"
                    ]
                , Tabs.label
                    [ Options.center
                    ]
                    [ text "Travis"
                    ]
                , Tabs.label
                    [ Options.center
                    ]
                    [ text "Import JSON"]
                ]
                []


viewAddBuild : Model -> List (Html Msg)
viewAddBuild model =
    let
        (rows, canSave) =
            case model.addBuildData.tab of
                0 ->
                    ( bambooRows model
                    , Bamboo.canSave model.addBuildData.bamboo
                    )
                1 ->
                    ( travisRows model
                    , Travis.canSave model.addBuildData.travis
                    )
                2 ->
                    ( importBuildRows model
                    , not <| String.isEmpty model.addBuildData.importText
                    )
                _ ->
                    Debug.crash "unhandled tab"
    in
        [ viewAddTabs model
        , Grid.grid
            []
            ( rows ++
                [ Grid.cell
                    [ Grid.size Grid.All 12 ]
                    [ Button.render Mdl [5, 0] model.mdl
                        [ Button.primary
                        , Button.raised
                        , Button.ripple
                        , Options.onClick <| AddBuildViewMsg ABOkClicked
                        , css "width" "100%"
                        , Button.disabled |> Options.when (not canSave)
                        ]
                        [ text <|
                            case model.addBuildData.editing of
                                Just b ->
                                    "Save"
                                Nothing ->
                                    "Add"
                        ]
                    ]
                ]
            )
        ]


onInputAbv : (String -> ABVMsg) -> String -> Msg
onInputAbv f s =
    AddBuildViewMsg (f s)


formRow : Html Msg -> Grid.Cell Msg
formRow h =
    Grid.cell
        [ Grid.size Grid.All 12
        , css "margin" "0"]
        [ h
        ]

tfOpts rest =
    [ Textfield.floatingLabel
    , Textfield.text_
    , css "width" "100%"
    ] ++ rest

withHelp : String -> Html Msg -> Html Msg
withHelp txt html =
    div
        []
        [ html
        , Options.styled p
            [ Typo.caption ]
            [ text txt ]
        ]

textfieldError maybeString =
    maybeString
        |> Maybe.map Textfield.error
        |> Maybe.withDefault Options.nop


bambooRows : Model -> List (Grid.Cell Msg)
bambooRows model =
    let
        bamboo =
            model.addBuildData.bamboo
        bambooErrors =
            model.addBuildData.bambooErrors
    in
        [ formRow <|
            withHelp "URL (and port if any) of the Bamboo server" <|
            Textfield.render Mdl [6, 0] model.mdl
                ( tfOpts
                    [ Textfield.label "Server URL"
                    , Textfield.value bamboo.serverUrl
                    , Options.onInput <| onInputAbv ABBambooServerUrlChanged
                    , textfieldError bambooErrors.serverUrl
                    ]
                )
                []
        , formRow <|
            withHelp "Plan key, usually looks like FOO-BAR" <|
            Textfield.render Mdl [6, 1] model.mdl
                ( tfOpts
                    [ Textfield.label "Plan"
                    , Textfield.value bamboo.plan
                    , Options.onInput <| onInputAbv ABBambooPlanChanged
                    , textfieldError bambooErrors.plan
                    ]
                )
                []
        , formRow <|
            withHelp "Bamboo username (if auth needed)" <|
            Textfield.render Mdl [6, 2] model.mdl
                ( tfOpts
                    [ Textfield.label "Username"
                    , Textfield.value bamboo.username
                    , Options.onInput <| onInputAbv ABBambooUsernameChanged
                    ]
                )
                []
        , formRow <|
            withHelp "Bamboo password (if auth needed)" <|
            Textfield.render Mdl [6, 3] model.mdl
                ( tfOpts
                    [ Textfield.label "Password"
                    , Textfield.value bamboo.password
                    , Options.onInput <| onInputAbv ABBambooPasswordChanged
                    ]
                )
                []
        ]


importBuildRows : Model -> List (Grid.Cell Msg)
importBuildRows model =
    let
        tf =
            Textfield.render Mdl [7] model.mdl
                ( tfOpts
                    [ Textfield.label "Paste JSON data here"
                    , Textfield.floatingLabel
                    , Textfield.value model.addBuildData.importText
                    , Textfield.textarea
                    , Textfield.rows 10
                    , Options.onInput <| onInputAbv ABImportTextChanged
                    , model.addBuildData.importError
                        |> Maybe.map Textfield.error
                        |> Maybe.withDefault Options.nop
                    ]
                )
                []
    in
        [ formRow <|
            case model.addBuildData.importError of
                Just err ->
                    tf
                Nothing ->
                    withHelp "JSON data is obtained by \"Sharing\" builds." tf
        ]


travisRows : Model -> List (Grid.Cell Msg)
travisRows model =
    let
        travis =
            model.addBuildData.travis
        travisErrors =
            model.addBuildData.travisErrors
    in
        [ formRow <|
            withHelp "URL of the Travis server (e.g. https://travis.org)" <|
            Textfield.render Mdl [8, 1] model.mdl
                ( tfOpts
                    [ Textfield.label "Server URL"
                    , Textfield.value travis.serverUrl
                    , Options.onInput <| onInputAbv ABTravisServerUrlChanged
                    , textfieldError travisErrors.serverUrl
                    ]
                )
                []
        , formRow <|
            withHelp "The repository (e.g. rails/rails)" <|
            Textfield.render Mdl [8, 2] model.mdl
                ( tfOpts
                    [ Textfield.label "Repository"
                    , Textfield.value travis.repository
                    , Options.onInput <| onInputAbv ABTravisRepoChanged
                    , textfieldError travisErrors.repository
                    ]
                )
                []
        , formRow <|
            withHelp "Branch to watch (e.g. master)" <|
            Textfield.render Mdl [8, 3] model.mdl
                ( tfOpts
                    [ Textfield.label "Branch"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value travis.branch
                    , Options.onInput <| onInputAbv ABTravisBranchChanged
                    , textfieldError travisErrors.branch
                    ]
                )
                []
        , formRow <|
            withHelp "GitHub token (if auth needed) " <|
            Textfield.render Mdl [8, 4] model.mdl
                ( tfOpts
                    [ Textfield.label "Token"
                    , Textfield.value travis.token
                    , Options.onInput <| onInputAbv ABTravisTokenChanged
                    ]
                )
                []
        ]


dialog : Model -> Html Msg
dialog model =
    case model.dialogKind of
        AboutDialog -> aboutDialog model
        PreferencesDialog -> preferencesDialog model
        FetchErrorDialog build -> fetchErrorDialog model build
        ShareBuildDialog builds -> shareBuildDialog model builds
        TagsDialog buildId tagsText -> tagsDialog model buildId tagsText
        TagDetailsDialog details -> tagDetailsDialog model details


aboutDialog : Model -> Html Msg
aboutDialog model =
    Dialog.view
        []
        [ Dialog.title [] [ text "About" ]
        , Dialog.content []
            [ p []
                [ text
                    <| "version : " ++ model.flags.appVersion
                ]
            , p []
                [ text
                    <| model.flags.appName ++ " monitors builds from several C.I. servers."
                ]
            , p []
                [ text "No more excuses for red builds !"
                ]
            , p
                []
                [ text "Open-Source, Hosted on "
                , a
                    [ href "#"
                    , onWithOptions
                        "click"
                        { preventDefault = True
                        , stopPropagation = True
                        }
                        (Json.succeed <| OpenUrl "https://github.com/vankeisb/build-watcher")
                    ]
                    [ text "GitHub"
                    ]
                ]
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 14 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Dismiss" ]
            ]
        ]

preferencesDialog : Model -> Html Msg
preferencesDialog model =
    let
        vSpacer =
            Options.div
                [ css "height" "18px"
                ]
                []
    in
        Dialog.view
            []
            [ Dialog.title [] [ text "Preferences" ]
            , Dialog.content []
                [ growLeft
                    ( label
                        []
                        [ text "Enable notifications"
                        ]
                    )
                    ( Toggles.switch Mdl [9, 1] model.mdl
                        [ Options.onToggle <| BuildsViewMsg BVPrefsToggleNotif
                        , Toggles.ripple
                        , Toggles.value model.preferences.enableNotifications
                        ]
                        []
                    )
                , vSpacer
                , Textfield.render Mdl [9, 2] model.mdl
                    [ Textfield.label "Polling interval (seconds)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value
                        <| toString model.preferences.pollingInterval
                    , Options.onInput
                        (\s -> BuildsViewMsg <| BVPrefsPollingChanged s)
                    ]
                    []
                , vSpacer
                , withHelp
                    ( "External tool is invoked on build status change."
                        ++ " Builds and tags statuses are passed as JSON."
                    ) <|
                    Textfield.render Mdl [9, 3] model.mdl
                        [ Textfield.label "Path to external tool"
                        , Textfield.floatingLabel
                        , Textfield.text_
                        , Textfield.value model.preferences.externalTool
                        , Options.onInput
                            (\s -> BuildsViewMsg <| BVPrefsExternalToolChanged s)
                        ]
                        []
                ]
            , Dialog.actions []
                [ Button.render Mdl
                    [ 9, 3 ]
                    model.mdl
                    [ Dialog.closeOn "click" ]
                    [ text "Done" ]
                ]
            ]


fetchErrorDialog : Model -> Build -> Html Msg
fetchErrorDialog model build =
    case build.fetchError of
        Just error ->
            Dialog.view
                []
                [ Dialog.title [] [ text "Error" ]
                , Dialog.content []
                    [ p []
                        [ text
                            <| "An error occured while fetching build results for "
                            ++ getBuildName build.def
                        ]
                    , code
                        []
                        [ text <| toString error
                        ]
                    ]
                , Dialog.actions []
                    [ Button.render Mdl
                        [ 18 ]
                        model.mdl
                        [ Dialog.closeOn "click" ]
                        [ text "Got it" ]
                    ]
                ]
        Nothing ->
            text ""


shareBuildDialog : Model -> List Build -> Html Msg
shareBuildDialog model builds =
    let
        json =
            Json.Encode.encode 4 <|
                Json.Encode.list <|
                    List.map
                    ( \b ->
                        Json.Encode.object <|
                            case b.def of
                            BambooDef cd d ->
                                Bamboo.encodeBambooData False d
                                    |> List.append [ encodeTags cd.tags ]
                            TravisDef cd d ->
                                Travis.encodeTravisData False d
                                    |> List.append [ encodeTags cd.tags ]
                    )
                    builds
    in
        Dialog.view
            []
            [ Dialog.title [] [ text "Share" ]
            , Dialog.content []
                [ p []
                    [ text
                        <| "Copy the data below, and send it via email/chat/whatever. "
                            ++ "The recipient will then be able to import it. "
                    ]
                , Textfield.render Mdl [19, 0] model.mdl
                    [ Textfield.textarea
                    , Textfield.rows 9
                    , Textfield.value json
                    , Options.id "export-data"
                    ]
                    []
                ]
            , Dialog.actions []
                [ div
                    [ style
                        [ displayFlex
                        ]
                    ]
                    [ div
                        [ style
                            [ flexGrow ]
                        ]
                        [ Button.render Mdl [ 19, 1 ] model.mdl
                            [ Options.onClick <| CopyToClipboard json
                            ]
                            [ text "Copy" ]
                        , Button.render Mdl [ 19, 2 ] model.mdl
                            [ Dialog.closeOn "click" ]
                            [ text "Dismiss" ]
                        ]
                    ]
                ]
            ]


tagsDialog : Model -> BuildId -> String -> Html Msg
tagsDialog model buildId tagsText =
    let
        build =
            getBuildById model buildId

        tags =
            case build of
                Just b ->
                    getCommonBuildData b.def
                        |> .tags
                        |> List.map (\tag ->
                            Chip.button
                                [ Chip.deleteIcon "cancel"
                                , Chip.deleteClick (BuildsViewMsg (BVDeleteTagClicked tag))
                                , css "margin" "5px 5px"
                                ]
                                [ Chip.content []
                                    [ text tag ]
                                ]
                        )

                Nothing ->
                    [ text "build not found"
                    ]
    in
        Dialog.view
            []
            [ Dialog.title [] [ text "Tags" ]
            , Dialog.content []
                [ div
                    [ style
                        [ displayFlex
                        , flexColumn
                        ]
                    ]
                    [ div
                        [ style
                            [ displayFlex
                            , flexGrow
                            , ( "flex-wrap", "wrap" )
                            ]
                        ]
                        tags
                    , Textfield.render Mdl [ 20, 0 ] model.mdl
                        [ Textfield.label "Add tag(s) and press ENTER"
                        , Textfield.floatingLabel
                        , Options.onInput <| (\s -> BuildsViewMsg (BVTagsChanged s))
                        , Textfield.value tagsText
                        , Options.on
                            "keydown"
                            ( Json.field "keyCode" Json.int
                                |> Json.map (\i -> BuildsViewMsg (BVTagsKeyUp i))
                            )
                        , css "width" "100%"
                        ]
                        []
                    ]
                ]
            , Dialog.actions []
                [ div
                    [ style
                        [ displayFlex
                        ]
                    ]
                    [ div
                        [ style
                            [ flexGrow ]
                        ]
                        [ Button.render Mdl [ 20, 1 ] model.mdl
                            [ Dialog.closeOn "click"
                            ]
                            [ text "Done" ]
                        ]
                    ]
                ]
            ]


tagDetailsDialog : Model -> TagDetailsData -> Html Msg
tagDetailsDialog model details =
    let
        txt b =
            text <| getBuildName b.def
        elem b =
            case b.result of
                Just result ->
                    a
                        [ href "#"
                        , onWithOptions
                            "click"
                            { preventDefault = True
                            , stopPropagation = True
                            }
                            (Json.succeed (OpenUrl result.url))
                        ]
                        [ txt b
                        ]
                Nothing ->
                    span
                        []
                        [ txt b ]
        buildElems builds title =
            if List.isEmpty builds then
                text ""
            else
                div
                    []
                    [ title
                    , ul
                        []
                        ( builds
                            |> List.map (\b ->
                                li
                                    []
                                    [ elem b ]
                            )
                        )
                    ]

        titleRow isGreen =
            Options.div
                [ Typo.title ]
                [ growRight
                    ( Icon.i <|
                        if isGreen then "mood" else "mood_bad"
                    )
                    ( text "Green" )
                ]
    in
        Dialog.view
            []
            [ Dialog.title [] [ text details.tag ]
            , Dialog.content []
                [ buildElems
                    details.redBuilds
                    ( growRight
                        (Icon.i "mood_bad")
                        ( Options.span [ Typo.title ] [ text "Red" ])
                    )
                , buildElems
                    details.greenBuilds
                    ( growRight
                        (Icon.i "mood")
                        (Options.span [ Typo.title ] [ text "Green" ])
                    )
                ]
            , Dialog.actions []
                [ div
                    [ style
                        [ displayFlex
                        ]
                    ]
                    [ div
                        [ style
                            [ flexGrow ]
                        ]
                        [ Button.render Mdl [ 21, 0 ] model.mdl
                            [ Dialog.closeOn "click"
                            ]
                            [ text "Done" ]
                        ]
                    ]
                ]
            ]
