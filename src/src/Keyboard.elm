module Keyboard exposing (..)


import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Keys exposing (Key, KeyColor(..), KeyKind(..))
import Note exposing (Note)



type Msg
    = Idle
    | SelectTonic Note


type alias Model =
    { keys : List Key
    }


view : Model -> Html Msg
view model =
    model.keys
        |> List.map viewKey
        |> div [ css [ displayFlex ] ]


viewKey : Key -> Html Msg
viewKey key =
    let
        coloringStyles =
            case key.kind of
                SelectedKey ->
                    Css.batch
                        [ backgroundColor (rgb 60 185 241)
                        , border3 (rem 0.05) solid (rgb 29 91 119)
                        ]

                StartKey ->
                    Css.batch
                        [ backgroundColor (rgb 123 236 55)
                        , border3 (rem 0.05) solid (rgb 72 138 32)
                        ]

                StopKey ->
                    Css.batch
                        [ backgroundColor (rgb 179 227 152)
                        , border3 (rem 0.05) solid (rgb 94 121 79)
                        ]

                NormalKey ->
                    if key.color == WhiteKey then
                        Css.batch
                            [ backgroundColor (rgb 255 255 255)
                            , border3 (rem 0.05) solid (rgb 4 4 4)
                            ]

                    else
                        Css.batch
                            [ backgroundColor (rgb 4 4 4)
                            , border3 (rem 0.05) solid (rgb 4 4 4)
                            ]

        commonStyle =
            Css.batch
                [ hover
                    [ backgroundColor (rgb 255 242 180)
                    , border3 (rem 0.05) solid (rgb 112 106 79)
                    ]
                , active
                    [ backgroundColor (rgb 255 224 66)
                    , border3 (rem 0.05) solid (rgb 100 88 26)
                    ]
                ]

        viewWhiteKey =
            div
                [ css
                    [ coloringStyles
                    , width (rem 1.2)
                    , height (rem 6)
                    , commonStyle
                    ]
                , onClick (SelectTonic key.note)
                ]
                []

        viewBlackKey =
            div
                [ css
                    [ position relative
                    ]
                ]
                [ div
                    [ css
                        [ coloringStyles
                        , width (rem 1)
                        , height (rem 3.6)
                        , position absolute
                        , left (rem -0.5)
                        , commonStyle
                        ]
                    , onClick (SelectTonic key.note)
                    ]
                    []
                ]
    in
    if key.color == WhiteKey then
        viewWhiteKey

    else
        viewBlackKey