module Fretboard exposing (..)


import Html.Styled exposing (..)
import Music exposing (Note)



type Msg
    = Idle


type alias Model =
    { notes : Note
    }


view : Model -> Html Msg
view model =
    div
        []
        [ text "hi"
        ]