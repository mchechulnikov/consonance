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
    | MajorSeventhChord Note
    | MinorSeventhChord Note
    | DominantSeventhChord Note
    | DiminishedSeventhChord Note
    | HalfDiminishedSeventhChord Note
    | MinorMajorSeventhChord Note
    | AugmentedMajorSeventhChord Note


toNotes : Chord -> List Note
toNotes chord =
    let getNotes tonic intervals = intervals |> Interval.structure tonic |> .notes in
    case chord of
        MajorChord tonic ->
            [ MajorThird, MinorThird ]
                |> getNotes tonic

        MinorChord tonic ->
            [ MinorThird, MajorThird ]
                |> getNotes tonic

        AugmentedChord tonic ->
            [ MajorThird, MajorThird ]
                |> getNotes tonic

        DiminishedChord tonic ->
            [ MinorThird, MinorThird ]
                |> getNotes tonic

        FourthChord tonic ->
            [ PerfectFourth, PerfectFifth ]
                |> getNotes tonic

        FifthChord tonic ->
            [ PerfectFifth, PerfectFourth ]
                |> getNotes tonic

        MajorSeventhChord tonic ->
            [ MajorThird, MinorThird, MajorThird ]
                |> getNotes tonic

        MinorSeventhChord tonic ->
            [ MinorThird, MajorThird, MinorThird ]
                |> getNotes tonic

        DominantSeventhChord tonic ->
            [ MajorThird, MinorThird, MinorThird ]
                |> getNotes tonic

        DiminishedSeventhChord tonic ->
            [ MinorThird, MinorThird, MinorThird ]
                |> getNotes tonic

        HalfDiminishedSeventhChord tonic ->
            [ MinorThird, MinorThird, MajorThird ]
                |> getNotes tonic

        MinorMajorSeventhChord tonic ->
            [ MinorThird, MajorThird, MajorThird ]
                |> getNotes tonic

        AugmentedMajorSeventhChord tonic ->
            [ MajorThird, MajorThird, MinorThird ]
                |> getNotes tonic


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

        MajorSeventhChord note ->
            (Note.toString note) ++ "M7"

        MinorSeventhChord note ->
            (Note.toString note) ++ "m7"

        DominantSeventhChord note ->
            (Note.toString note) ++ "7"

        DiminishedSeventhChord note ->
            (Note.toString note) ++ "dim7"

        HalfDiminishedSeventhChord note ->
            (Note.toString note) ++ "m7♭5"

        MinorMajorSeventhChord note ->
            (Note.toString note) ++ "mM7"

        AugmentedMajorSeventhChord note ->
            (Note.toString note) ++ "M7♯5"


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

        MajorSeventhChord _ ->
            "major seventh"

        MinorSeventhChord _ ->
            "minor seventh"

        DominantSeventhChord _ ->
            "dominant seventh"

        DiminishedSeventhChord _ ->
            "diminished seventh"

        HalfDiminishedSeventhChord _ ->
            "half diminished seventh"

        MinorMajorSeventhChord _ ->
            "minor major seventh"

        AugmentedMajorSeventhChord _ ->
            "augmented major seventh"



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

        "major seventh" ->
            Just MajorSeventhChord

        "minor seventh" ->
            Just MinorSeventhChord

        "dominant seventh" ->
            Just DominantSeventhChord

        "diminished seventh" ->
            Just DiminishedSeventhChord

        "half diminished seventh" ->
            Just HalfDiminishedSeventhChord

        "minor major seventh" ->
            Just MinorMajorSeventhChord

        "augmented major seventh" ->
            Just AugmentedMajorSeventhChord

        _ ->
            Nothing
