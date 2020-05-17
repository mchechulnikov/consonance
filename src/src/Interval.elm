module Interval exposing (..)

import List exposing (reverse)
import Note exposing (Note)
import Tuple exposing (second)



type Interval
    = PerfectUnison Note | DiminishedSecond Note
    | MinorSecond Note | AugmentedUnison Note
    | MajorSecond Note | DiminishedThird Note
    | MinorThird Note | AugmentedSecond Note
    | MajorThird Note | DiminishedFourth Note
    | PerfectFourth Note | AugmentedThird Note
    | AugmentedFourth Note
    | DiminishedFifth Note
    | PerfectFifth Note | DiminishedSixth Note
    | MinorSixth Note | AugmentedFifth Note
    | MajorSixth Note | DiminishedSeventh Note
    | MinorSeventh Note | AugmentedSixth Note
    | MajorSeventh Note | DiminishedOctave Note
    | PerfectOctave Note | AugmentedSeventh Note


getSemitonesNumbers : Interval -> Int
getSemitonesNumbers interval =
    case interval of
        PerfectUnison _ ->
            0

        DiminishedSecond _ ->
            0

        MinorSecond _ ->
            1

        AugmentedUnison _ ->
            1

        MajorSecond _ ->
            2

        DiminishedThird _ ->
            2

        MinorThird _ ->
            3

        AugmentedSecond _ ->
            3

        MajorThird _ ->
            4

        DiminishedFourth _ ->
            4

        PerfectFourth _ ->
            5

        AugmentedThird _ ->
            5

        AugmentedFourth _ ->
            6

        DiminishedFifth _ ->
            6

        PerfectFifth _ ->
            7

        DiminishedSixth _ ->
            7

        MinorSixth _ ->
            8

        AugmentedFifth _ ->
            8

        MajorSixth _ ->
            9

        DiminishedSeventh _ ->
            9

        MinorSeventh _ ->
            10

        AugmentedSixth _ ->
            10

        MajorSeventh _ ->
            11

        DiminishedOctave _ ->
            11

        PerfectOctave _ ->
            12

        AugmentedSeventh _ ->
            12


toKindString : Interval -> String
toKindString interval =
    case interval of
        PerfectUnison _ ->
            "P1"

        DiminishedSecond _ ->
            "d2"

        MinorSecond _ ->
            "m2"

        AugmentedUnison _ ->
            "A1"

        MajorSecond _ ->
            "M2"

        DiminishedThird _ ->
            "d3"

        MinorThird _ ->
            "m3"

        AugmentedSecond _ ->
            "A2"

        MajorThird _ ->
            "M3"

        DiminishedFourth _ ->
            "d4"

        PerfectFourth _ ->
            "P4"

        AugmentedThird _ ->
            "A3"

        AugmentedFourth _ ->
            "A4"

        DiminishedFifth _ ->
            "d5"

        PerfectFifth _ ->
            "P5"

        DiminishedSixth _ ->
            "d6"

        MinorSixth _ ->
            "m6"

        AugmentedFifth _ ->
            "A5"

        MajorSixth _ ->
            "M6"

        DiminishedSeventh _ ->
            "d7"

        MinorSeventh _ ->
            "m7"

        AugmentedSixth _ ->
            "A6"

        MajorSeventh _ ->
            "M7"

        DiminishedOctave _ ->
            "d8"

        PerfectOctave _ ->
            "P8"

        AugmentedSeventh _ ->
            "A7"


fromKindString : String -> Maybe (Note -> Interval)
fromKindString string =
    case string of
        "P1" ->
            Just PerfectUnison

        "d2" ->
            Just DiminishedSecond

        "m2" ->
            Just MinorSecond

        "A1" ->
            Just AugmentedUnison

        "M2" ->
            Just MajorSecond

        "d3" ->
            Just DiminishedThird

        "m3" ->
            Just MinorThird

        "A2" ->
            Just AugmentedSecond

        "M3" ->
            Just MajorThird

        "d4" ->
            Just DiminishedFourth

        "P4" ->
            Just PerfectFourth

        "A3" ->
            Just AugmentedThird

        "A4" ->
            Just AugmentedFourth

        "d5" ->
            Just DiminishedFifth

        "P5" ->
            Just PerfectFifth

        "d6" ->
            Just DiminishedSixth

        "m6" ->
            Just MinorSixth

        "A5" ->
            Just AugmentedFifth

        "M6" ->
            Just MajorSixth

        "d7" ->
            Just DiminishedSeventh

        "m7" ->
            Just MinorSeventh

        "A6" ->
            Just AugmentedSixth

        "M7" ->
            Just MajorSeventh

        "d8" ->
            Just DiminishedOctave

        "P8" ->
            Just PerfectOctave

        "A7" ->
            Just AugmentedSeventh

        _ ->
            Nothing


structure : Note -> List (Note -> Interval) -> { intervals : List Interval, notes : List Note }
structure startNote intervalFunctions =
    intervalFunctions
       |> List.foldl
           (\intervalFunc (note, { intervals, notes }) ->
               let
                   interval = intervalFunc note
                   nextNote = note |> Note.plusSemitones (getSemitonesNumbers interval)
               in
               (nextNote, { intervals = interval :: intervals, notes = nextNote :: notes })
           )
           (startNote, { intervals = [], notes = [ startNote ] })
       |> second
       |> (\x -> { x | intervals = reverse x.intervals, notes = reverse x.notes })


toNotes : Interval -> List Note
toNotes interval =
    let getNotes tonic intervals = intervals |> structure tonic |> .notes in
    case interval of
        PerfectUnison tonic ->
            getNotes tonic [ PerfectUnison ]

        DiminishedSecond tonic ->
            getNotes tonic [ DiminishedSecond ]

        MinorSecond tonic ->
            getNotes tonic [ MinorSecond ]

        AugmentedUnison tonic ->
            getNotes tonic [ AugmentedUnison ]

        MajorSecond tonic ->
            getNotes tonic [ MajorSecond ]

        DiminishedThird tonic ->
            getNotes tonic [ DiminishedThird ]

        MinorThird tonic ->
            getNotes tonic [ MinorThird ]

        AugmentedSecond tonic ->
            getNotes tonic [ AugmentedSecond ]

        MajorThird tonic ->
            getNotes tonic [ MajorThird ]

        DiminishedFourth tonic ->
            getNotes tonic [ DiminishedFourth ]

        PerfectFourth tonic ->
            getNotes tonic [ PerfectFourth ]

        AugmentedThird tonic ->
            getNotes tonic [ AugmentedThird ]

        AugmentedFourth tonic ->
            getNotes tonic [ AugmentedFourth ]

        DiminishedFifth tonic ->
            getNotes tonic [ DiminishedFifth ]

        PerfectFifth tonic ->
            getNotes tonic [ PerfectFifth ]

        DiminishedSixth tonic ->
            getNotes tonic [ DiminishedSixth ]

        MinorSixth tonic ->
            getNotes tonic [ MinorSixth ]

        AugmentedFifth tonic ->
            getNotes tonic [ AugmentedFifth ]

        MajorSixth tonic ->
            getNotes tonic [ MajorSixth ]

        DiminishedSeventh tonic ->
            getNotes tonic [ DiminishedSeventh ]

        MinorSeventh tonic ->
            getNotes tonic [ MinorSeventh ]

        AugmentedSixth tonic ->
            getNotes tonic [ AugmentedSixth ]

        MajorSeventh tonic ->
            getNotes tonic [ MajorSeventh ]

        DiminishedOctave tonic ->
            getNotes tonic [ DiminishedOctave ]

        PerfectOctave tonic ->
            getNotes tonic [ PerfectOctave ]

        AugmentedSeventh tonic ->
            getNotes tonic [ AugmentedSeventh ]