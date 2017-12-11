module View exposing (..)


import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Material.Button as Button
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as Lists
import Material.Menu as Menu
import Material.Options as Options exposing (css)
import Material.Scheme
import Material.Snackbar as Snackbar
import Material.Tabs as Tabs
import Material.Textfield as Textfield
import Material.Typography as Typo
import Messages exposing (..)
import Model exposing (..)
import Style exposing (..)
import Json.Decode as Json

i name =
    Icon.view name [ css "width" "40px" ]

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
                    , span [] [ text <| model.flags.appName ]
                    , span []
                        [ case model.view of
                            BuildListView ->
                                text ""
                            AddBuildView ->
                                case model.addBuildData.editing of
                                    Just build ->
                                        text " / edit"
                                    Nothing ->
                                        text " / add"
                        ]
                    ]
                , Layout.spacer
                , case model.view of
                    BuildListView ->
                        Menu.render Mdl [0] model.mdl
                            [ Menu.bottomRight
                            , Menu.ripple
                            ]
                            [ Menu.item
                                [ Menu.onSelect <| BuildsViewMsg BVAddBuildClicked
                                , padding
                                ]
                                [ i "add_circle"
                                , text "Add build..."
                                ]
                            , Menu.item
                                [ Dialog.openOn "click"
                                , padding
                                ]
                                [ i "help"
                                , text "About"
                                ]
                            ]

                    AddBuildView ->
                        text ""
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
                [ aboutDialog model
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
            case b.result of
                Just result ->
                    Options.attribute
                        <| Html.Events.onClick (OpenUrl result.url)
                Nothing ->
                    Options.nop

        (browseEnabledAttr, browseSelectAttr) =
            case b.result of
                Just result ->
                    ( Options.nop
                    , Menu.onSelect <| OpenUrl result.url
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
                            BambooDef d ->
                                d.serverUrl
                            TravisDef d ->
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
                                "Welcome to " ++ model.flags.appName ++ " ! Add builds using the top-right menu."
                            else
                                "No builds are monitored. Add builds using the top-right menu."
                        ]
                    ]
                ]
        else
            Lists.ul
                []
                ( model.builds
                    |> List.indexedMap (viewDefAndResult model)
                )
    ]


viewAddTabs : Model -> Html Msg
viewAddTabs model =
    case model.addBuildData.editing of
        Just build ->
            text ""
        Nothing ->
            Tabs.render Mdl [1] model.mdl
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
                ]
                []


viewAddBuild : Model -> List (Html Msg)
viewAddBuild model =
    let
        rows =
            if model.addBuildData.tab == 0 then
                bambooRows model
            else
                travisRows model
    in
        [ viewAddTabs model
        , Grid.grid
            []
            ( rows ++
                [ Grid.cell
                    [ Grid.size Grid.All 6 ]
                    [ Button.render Mdl [10] model.mdl
                        [ Button.primary
                        , Button.raised
                        , Button.ripple
                        , Options.onClick <| AddBuildViewMsg ABOkClicked
                        , css "width" "100%"
                        ]
                        [ text <|
                            case model.addBuildData.editing of
                                Just b ->
                                    "Save"
                                Nothing ->
                                    "Add"
                        ]
                    ]
                , Grid.cell
                    [ Grid.size Grid.All 6 ]
                    [ Button.render Mdl [11] model.mdl
                        [ Button.flat
                        , Button.ripple
                        , Options.onClick <| AddBuildViewMsg ABCancelClicked
                        , css "width" "100%"
                        ]
                        [ text "Cancel" ]
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


bambooRows : Model -> List (Grid.Cell Msg)
bambooRows model =
    [ formRow <|
        Textfield.render Mdl [2] model.mdl
            ( tfOpts
                [ Textfield.label "Server URL"
                , Textfield.value model.addBuildData.bamboo.serverUrl
                , Options.onInput <| onInputAbv ABBambooServerUrlChanged
                ]
            )
            []
    , formRow <|
        Textfield.render Mdl [3] model.mdl
            ( tfOpts
                [ Textfield.label "Plan"
                , Textfield.value model.addBuildData.bamboo.plan
                , Options.onInput <| onInputAbv ABBambooPlanChanged
                ]
            )
            []
    , formRow <|
        Textfield.render Mdl [4] model.mdl
            ( tfOpts
                [ Textfield.label "Username"
                , Textfield.value model.addBuildData.bamboo.username
                , Options.onInput <| onInputAbv ABBambooUsernameChanged
                ]
            )
            []
    , formRow <|
        Textfield.render Mdl [5] model.mdl
            ( tfOpts
                [ Textfield.label "Password"
                , Textfield.value model.addBuildData.bamboo.password
                , Options.onInput <| onInputAbv ABBambooPasswordChanged
                ]
            )
            []
    ]


travisRows : Model -> List (Grid.Cell Msg)
travisRows model =
    [ formRow <|
        Textfield.render Mdl [6] model.mdl
            ( tfOpts
                [ Textfield.label "Server URL"
                , Textfield.value model.addBuildData.travis.serverUrl
                , Options.onInput <| onInputAbv ABTravisServerUrlChanged
                ]
            )
            []
    , formRow <|
        Textfield.render Mdl [7] model.mdl
            ( tfOpts
                [ Textfield.label "Repository"
                , Textfield.value model.addBuildData.travis.repository
                , Options.onInput <| onInputAbv ABTravisRepoChanged
                ]
            )
            []
    , formRow <|
        Textfield.render Mdl [8] model.mdl
            ( tfOpts
                [ Textfield.label "Branch"
                , Textfield.floatingLabel
                , Textfield.text_
                , Textfield.value model.addBuildData.travis.branch
                , Options.onInput <| onInputAbv ABTravisBranchChanged
                ]
            )
            []
    , formRow <|
        Textfield.render Mdl [9] model.mdl
            ( tfOpts
                [ Textfield.label "Token"
                , Textfield.value model.addBuildData.travis.token
                , Options.onInput <| onInputAbv ABTravisTokenChanged
                ]
            )
            []
    ]


aboutDialog : Model -> Html Msg
aboutDialog model =
    Dialog.view
        []
        [ Dialog.title [] [ text "About" ]
        , Dialog.content []
            [ p []
                [ text
                    <| model.flags.appName ++ " monitors builds from several C.I. servers."
                ]
            , p []
                [ text
                    <| "version : " ++ model.flags.appVersion
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
                [ 20 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Dismiss" ]
            ]
        ]
