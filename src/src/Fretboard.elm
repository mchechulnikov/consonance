module Fretboard exposing (..)


import Guitar exposing (FretPoint, Guitar, GuitarFret, GuitarString(..), Tuning, unwindTuning)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Note exposing (Note)



type Msg
    = Idle


type alias Model =
    { guitar : Guitar
    , selectedFretPoints : List FretPoint
    }


type FretPointState
    = NormalFretPoint
    | SelectedFretPoint


view : Model -> Html Msg
view model =
    let
        selectedFretsOnString stringNumber =
            model.selectedFretPoints
                |> List.filter (.string >> (==) (GuitarString stringNumber))
                |> List.map .fret

        viewGuitarString stringNumber string =
            viewString
                (selectedFretsOnString (stringNumber + 1))
                model.guitar.fretsNumber
                string
    in
    model.guitar.tuning
        |> unwindTuning
        |> List.indexedMap viewGuitarString
        |> div
            []


viewString : List GuitarFret -> Int -> Note -> Html Msg
viewString selectedFrets fretNumbers stringTuning =
    let
        viewGuitarFretPoint fretNumber note =
            if selectedFrets |> List.member (Guitar.GuitarFret fretNumber) then
                viewFretPoint SelectedFretPoint note

            else
                viewFretPoint NormalFretPoint note
    in
    Guitar.generateStringLayout fretNumbers stringTuning
        |> List.indexedMap viewGuitarFretPoint
        |> div
            [ css
                [ displayFlex
                ]
            ]


viewFretPoint : FretPointState -> Note -> Html Msg
viewFretPoint fretPointState note =
    let
        backgroundStyle =
            case fretPointState of
                NormalFretPoint ->
                    backgroundColor (rgb 255 255 255)

                SelectedFretPoint ->
                    backgroundColor (rgba 83 217 1 0.75)
    in
    div
        [ css
            [ displayFlex
            , padding (rem 1)
            , width (rem 2)
            , cursor pointer
            , backgroundStyle
            , hover
                [ backgroundColor (rgba 255 213 0 0.2431)
                ]
            ]
        ]
        [ note |> Note.toString |> text ]