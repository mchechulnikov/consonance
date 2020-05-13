module Fretboard exposing (..)


import Guitar exposing (FretPoint, Guitar, Tuning, unwindTuning)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, css)
import Html.Styled.Events exposing (..)
import Note exposing (Note)



type Msg
    = Idle


type alias Model =
    { guitar : Guitar
    , selectedFretPoints : List FretPoint
    }


type FretPointState
    = NormalFretPoint
    | ClampedFretPoint


view : Model -> Html Msg
view model =
    model.guitar.tuning
        |> unwindTuning
        |> List.map (viewString model.guitar.fretsNumber)
        |> div
            []


viewString : Int -> Note -> Html Msg
viewString fretNumbers stringTuning =
    Guitar.generateStringLayout fretNumbers stringTuning
        |> List.map (viewFretPoint NormalFretPoint)
        |> div
            [ css
                [ displayFlex
                ]
            ]


viewFretPoint : FretPointState -> Note -> Html Msg
viewFretPoint _ note =
    div
        [ css
            [ displayFlex
            , padding (rem 1)
            , width (rem 2)
            , cursor pointer
            , hover
                [ backgroundColor (rgba 128 128 128 0.25)
                ]
            ]
        ]
        [ note |> Note.toString |> text ]