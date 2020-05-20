module Fretboard exposing (..)


import Guitar exposing (FretPoint, FretPointKind(..), Guitar, GuitarFret, GuitarString(..), Tuning, unwindTuning)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import List exposing (singleton)
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

        viewGuitarString stringNumber string =
            viewString
                (selectedFretsOnString (stringNumber + 1))
                string

        fretNumbersRow =
            let
                gen n acc =
                    if n >= 0 then
                        gen (n - 1) (n :: acc)
                    else
                        acc
            in
            gen model.guitar.fretsNumber []
                |> List.map (String.fromInt >> text >> singleton)
                |> List.map (div [ css [ displayFlex, padding (rem 1), width (rem 2) ] ])
                |> div [ css [ displayFlex ] ]
    in
    div
        []
        [ fretNumbersRow
        , model.guitar.layout
            |> List.indexedMap viewGuitarString
            |> div []
        ]


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
                    backgroundColor (rgb 60 185 241)

                StartFretPoint ->
                    backgroundColor (rgb 123 236 55)

                StopFretPoint ->
                    backgroundColor (rgb 179 227 152)
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
            , active
                [ backgroundColor (rgba 255 213 0 0.749)
                ]
            ]
        , onClick (SelectTonic note)
        ]
        [ note |> Note.toString |> text ]