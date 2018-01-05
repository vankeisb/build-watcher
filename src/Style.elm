module Style exposing (..)

import Html.Attributes exposing (style)
import Html exposing (div)
import Material.Options


displayFlex = ("display", "flex")
position s = ("position", s)
positionRelative = position "relative"
positionAbsolute = position "absolute"
top0 = ("top", "0")
left0 = ("left", "0")
right0 = ("right", "0")
bottom0 = ("bottom", "0")
alignItemsCenter = ("align-items", "center")
flexGrow = ("flex-grow", "1")
flexColumn = ("flex-direction", "column")
flexWrap = ("flex-wrap", "wrap")


growLeftOrRight growLeft left right =
    div
        [ style
            [ displayFlex
            , alignItemsCenter
            , flexGrow
            ]
        ]
        [ div
            [ style
                (
                    [ displayFlex
                    , alignItemsCenter
                    ] ++
                        if growLeft then
                            [ flexGrow ]
                        else []
                )
            ]
            [ left
            ]
        , div
            [ style
                (
                    [ displayFlex
                    , alignItemsCenter
                    ] ++
                        if not growLeft then
                            [ flexGrow ]
                        else []
                )
            ]
            [ right
            ]
        ]


growRight =
    growLeftOrRight False


growLeft =
    growLeftOrRight True


wordWrapStyles =
    [ ("text-overflow", "ellipsis")
    , ("white-space", "nowrap")
    , ("overflow", "hidden")
    ]

wordWrap =
    wordWrapStyles
        |> List.map (\(n,v) -> Material.Options.css n v)
