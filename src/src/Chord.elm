module Chord exposing (..)

import Interval exposing (Interval(..))
import Note exposing (..)



type Chord
    = MajorChord Note
    | MinorChord Note
    | AugmentedChord Note
    | DiminishedChord Note
    | FourthChord Note
    | FifthChord Note


toNotes : Chord -> List Note
toNotes chord =
    case chord of
        MajorChord tonic ->
            [ MajorThird, MinorThird ]
                |> Interval.structure tonic
                |> .notes

        MinorChord tonic ->
            [ MinorThird, MajorThird ]
                |> Interval.structure tonic
                |> .notes

        AugmentedChord tonic ->
            [ MajorThird, MajorThird ]
                |> Interval.structure tonic
                |> .notes

        DiminishedChord tonic ->
            [ MinorThird, MinorThird ]
                |> Interval.structure tonic
                |> .notes

        FourthChord tonic ->
            [ PerfectFourth, PerfectFifth ]
                |> Interval.structure tonic
                |> .notes

        FifthChord tonic ->
            [ PerfectFifth, PerfectFourth ]
                |> Interval.structure tonic
                |> .notes


toString : Chord -> String
toString chord =
    case chord of
        MajorChord note ->
            Note.toString note

        MinorChord note ->
            (Note.toString note) ++ "m"

        AugmentedChord note ->
            (Note.toString note) ++ "aug"

        DiminishedChord note ->
            (Note.toString note) ++ "dim"

        FourthChord note ->
            (Note.toString note) ++ "4"

        FifthChord note ->
            (Note.toString note) ++ "5"


toKindString : Chord -> String
toKindString chord =
    case chord of
        MajorChord _ ->
            "major"

        MinorChord _ ->
            "minor"

        AugmentedChord _ ->
            "augmented"

        DiminishedChord _ ->
            "diminished"

        FourthChord _ ->
            "fourth"

        FifthChord _ ->
            "fifth"



fromKindString : String -> Maybe (Note -> Chord)
fromKindString val =
    case val of
        "major" ->
            Just MajorChord

        "minor" ->
            Just MinorChord

        "augmented" ->
            Just AugmentedChord

        "diminished" ->
            Just DiminishedChord

        "fourth" ->
            Just FourthChord

        "fifth" ->
            Just FifthChord

        _ ->
            Nothing
