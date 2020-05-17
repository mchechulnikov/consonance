module Fretboard exposing (..)


import Guitar exposing (FretPoint, FretPointKind(..), Guitar, GuitarFret, GuitarString(..), Tuning, unwindTuning)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import List.Extra as List
import Note exposing (Note)



type Msg
    = Idle
    | SelectTonic Note


type alias Model =
    { guitar : Guitar
    , selectedFretPoints : List FretPoint
    }


view : Model -> Html Msg
view model =
    let
        selectedFretsOnString stringNumber =
            model.selectedFretPoints
                |> List.filter (.string >> (==) (GuitarString stringNumber))
                --|> List.map .fret

        viewGuitarString stringNumber string =
            viewString
                (selectedFretsOnString (stringNumber + 1))
                string
    in
    model.guitar.layout
        |> List.indexedMap viewGuitarString
        |> div
            []


viewString : List FretPoint -> List Note -> Html Msg
viewString selectedFrets stringLayout =
    let
        viewGuitarFretPoint fretNumber note =
            selectedFrets
                |> List.find (.fret >> (==) (Guitar.GuitarFret fretNumber))
                |> Maybe.map .kind
                |> Maybe.withDefault NormalFretPoint
                |> viewFretPoint note
    in
    stringLayout
        |> List.indexedMap viewGuitarFretPoint
        |> div [ css [ displayFlex ] ]


viewFretPoint : Note -> FretPointKind -> Html Msg
viewFretPoint note fretPointState  =
    let
        backgroundStyle =
            case fretPointState of
                NormalFretPoint ->
                    backgroundColor (rgb 255 255 255)

                SelectedFretPoint ->
                    backgroundColor (rgba 1 149 217 0.749)

                StartFretPoint ->
                    backgroundColor (rgba 83 217 1 0.75)

                StopFretPoint ->
                    backgroundColor (rgba 83 217 1 0.502)
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
        , onClick (SelectTonic note)
        ]
        [ note |> Note.toString |> text ]