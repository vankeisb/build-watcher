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
        ]
        { header =
            [ Layout.row
                [ css "padding-left" "16px"
                ]
                [ Layout.title
                    [ css "align-items" "center"
                    , css "display" "flex"
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
                , Layout.spacer
                , case model.view of
                    BuildListView ->
                        div
                            [ style
                                [ displayFlex
                                , alignItemsCenter
                                ]
                            ]
                            [
                                if not model.filterVisible then
                                    Button.render Mdl [0, 0] model.mdl
                                        [ Button.icon
                                        , Button.ripple
                                        , Options.onClick <| BuildsViewMsg BVShowFilterClicked
                                        ]
                                        [ Icon.i "search"
                                        ]
                                else
                                    text ""
                            , Menu.render Mdl [0, 1] model.mdl
                                [ Menu.bottomRight
                                , Menu.ripple
                                ]
                                [ Menu.item
                                    [ Menu.onSelect <| BuildsViewMsg BVAddBuildClicked
                                    ]
                                    [ i "add"
                                    , text "Add builds"
                                    ]
                                , Menu.item
                                    [ Dialog.openOn "click"
                                    , Menu.onSelect <| BuildsViewMsg BVShareAllClicked
                                    , padding
                                    ]
                                    [ i "share"
                                    , text "Share builds"
                                    ]
                                , Menu.item
                                    [ Dialog.openOn "click"
                                    , Menu.onSelect <| BuildsViewMsg BVPrefsClicked
                                    , padding
                                    , Menu.divider
                                    ]
                                    [ i "build"
                                    , text "Preferences"
                                    ]
                                , Menu.item
                                    [ Dialog.openOn "click"
                                    , Menu.onSelect <| BuildsViewMsg BVAboutClicked
                                    , padding
                                    ]
                                    [ i "help"
                                    , text "About"
                                    ]
                                , Menu.item
                                    [ Menu.onSelect <| BuildsViewMsg BVQuitClicked
                                    , padding
                                    ]
                                    [ i "close"
                                    , text "Quit"
                                    ]
                                ]
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
        , drawer = []
        , tabs = ([], [])
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
        |> Material.Scheme.topWithScheme Color.BlueGrey Color.Blue


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

        wordWrap =
            [ css "text-overflow" "ellipsis"
            , css "white-space" "nowrap"
            , css "overflow" "hidden"
            ]

        padding =
            css "padding-right" "18px"

        liClick =
            Options.onClick (BuildsViewMsg (BVBuildClicked b))

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
    in
        Lists.li
            [ Lists.withSubtitle
            , css "overflow" "inherit"
            ]
            [ Lists.content
                [ liClick
                , b.fetchError
                    |> Maybe.map (\_ -> Dialog.openOn "click")
                    |> Maybe.withDefault Options.nop
                , cs "bm-clickable"
                ]
                [ Lists.avatarIcon
                    avatarIcon
                    [ Color.background (Color.color hue Color.S500) ]
                , Options.div
                    wordWrap
                    [ text <| getBuildName b.def
                    ]
                , Lists.subtitle
                    wordWrap
                    [ text <|
                        case b.def of
                            BambooDef _ d ->
                                d.serverUrl
                            TravisDef _ d ->
                                d.serverUrl
                    ]
                ]
            , Menu.render Mdl [ 1, index ] model.mdl
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
                            [ Lists.ul []
                                ( model.builds
                                    |> List.filter (\b -> not b.filtered)
                                    |> List.indexedMap
                                        (viewDefAndResult model)
                                )
                            ]
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
            if model.filterVisible then
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
    [ formRow <|
        withHelp "Build JSON can is obtained by \"Sharing\" builds." <|
        Textfield.render Mdl [7] model.mdl
            ( tfOpts
                [ Textfield.label "Paste JSON data here"
                , Textfield.floatingLabel
                , Textfield.value model.addBuildData.importText
                , Textfield.textarea
                , Textfield.rows 10
                , Options.onInput <| onInputAbv ABImportTextChanged
                ]
            )
            []
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
            withHelp "Travis token if needed (as provided by the travis cmd line)" <|
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
            , Options.div
                [ css "height" "18px"
                ]
                []
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
                    ( \b -> case b.def of
                        BambooDef _ d ->
                            Bamboo.encodeBambooData False d
                        TravisDef _ d ->
                            Travis.encodeTravisData False d
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
                , Textfield.render Mdl [9] model.mdl
                    [ Textfield.textarea
                    , Textfield.rows 9
                    , Textfield.value json
                    , Options.id "export-data"
                    ]
                    []
                , model.addBuildData.importError
                    |> Maybe.map (\importError ->
                        p []
                            [ text <|
                                "Error importing project : " ++ (toString importError)
                            ]
                    )
                    |> Maybe.withDefault (text "")
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
                        [ Button.render Mdl [ 19 ] model.mdl
                            [ Options.onClick <| CopyToClipboard json
                            ]
                            [ text "Copy" ]
                        , Button.render Mdl [ 20 ] model.mdl
                            [ Dialog.closeOn "click" ]
                            [ text "Dismiss" ]
                        ]
                    ]
                ]
            ]
