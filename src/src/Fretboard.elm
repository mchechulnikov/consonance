module Fretboard exposing (..)



import Html.Styled exposing (..)


type Msg
    = Idle


type alias Model =
    {}


view : Model -> Html Msg
view model =
    div
        []
        [ text "hi"
        ]