module Scale exposing (..)

import Interval exposing (Interval(..))
import Note exposing (Note)


type Scale
    = MajorPentatonicScale Note
    | MinorPentatonicScale Note


toNotes : Scale -> List Note
toNotes scale =
    case scale of
        MajorPentatonicScale tonic ->
            [ MajorSecond, MajorSecond, MinorThird, MajorSecond, MinorThird ]
                |> Interval.structure tonic
                |> .notes

        MinorPentatonicScale tonic ->
            [ MinorThird, MajorSecond, MajorSecond, MinorThird, MajorSecond ]
                |> Interval.structure tonic
                |> .notes


toKindString : Scale -> String
toKindString scale =
    case scale of
        MajorPentatonicScale _ ->
            "major pentatonic"

        MinorPentatonicScale _ ->
            "minor pentatonic"


fromKindString : String -> Maybe (Note -> Scale)
fromKindString val =
    case val of
        "major pentatonic" ->
            Just MajorPentatonicScale

        "minor pentatonic" ->
            Just MinorPentatonicScale

        _ ->
            Nothing