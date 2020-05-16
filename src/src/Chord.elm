module Chord exposing (..)

import Interval exposing (Interval(..))
import Note exposing (..)



type Chord
    = MajorChord Note
    | MinorChord Note
    | AugmentedChord Note
    | DiminishedChord Note


toNotes : Chord -> List Note
toNotes chord =
    let
        (interval1, interval2) =
            chordStructure chord

        first =
            case chord of
                MajorChord tonic ->
                    tonic

                MinorChord tonic ->
                    tonic

                AugmentedChord tonic ->
                    tonic

                DiminishedChord tonic ->
                    tonic

        second =
            first |> Note.plusSemitones (Interval.getSemitonesNumbers interval1)

        third =
            second |> Note.plusSemitones (Interval.getSemitonesNumbers interval2)
    in
    [ first, second, third]


fromIntervals : Interval -> Interval -> Maybe Chord
fromIntervals interval1 interval2 =
    case (interval1, interval2) of
        (MinorThird a, MajorThird _) ->
            Just (MajorChord a)

        (MajorThird a, MinorThird _) ->
            Just (MinorChord a)

        (MinorThird a, MinorThird _) ->
            Just (DiminishedChord a)

        (MajorThird a, MajorThird _) ->
            Just (AugmentedChord a)

        (_, _) ->
            Nothing


chordStructure : Chord -> (Interval, Interval)
chordStructure chord =
    let
        compute tonic interval1 interval2 =
            let
                first = interval1 tonic
                second =
                    tonic
                        |> Note.plusSemitones (Interval.getSemitonesNumbers first)
                        |> interval2
            in
            (first, second)
    in
    case chord of
        MajorChord tonic ->
            compute tonic MajorThird MinorThird

        MinorChord tonic ->
            compute tonic MinorThird MajorThird

        AugmentedChord tonic ->
            compute tonic MajorThird MajorThird

        DiminishedChord tonic ->
            compute tonic MinorThird MinorThird