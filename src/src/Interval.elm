module Interval exposing (..)

import Note exposing (Note)



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


toString : Interval -> String
toString interval =
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