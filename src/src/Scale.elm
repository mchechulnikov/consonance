module Scale exposing (..)

import Interval exposing (Interval(..))
import Note exposing (Note)



type Scale
    = MajorPentatonicScale Note
    | MinorPentatonicScale Note


toNotes : Scale -> List Note
toNotes scale =
    case scale of
        MajorPentatonicScale _ ->
            scale
                |> pentatonicStructure
                |> List.map Interval.extractNote

        MinorPentatonicScale _ ->
            scale
                |> pentatonicStructure
                |> List.map Interval.extractNote


pentatonicStructure : Scale -> List Interval
pentatonicStructure chord =
    case chord of
        MajorPentatonicScale tonic ->
            Interval.structure
                tonic
                [ MajorSecond, MajorSecond, MinorThird, MajorSecond, MinorThird ]

        MinorPentatonicScale tonic ->
            Interval.structure
                tonic
                [ MinorThird, MajorSecond, MajorSecond, MinorThird, MajorSecond ]
