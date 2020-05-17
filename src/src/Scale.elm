module Scale exposing (..)

import Interval exposing (Interval(..))
import Note exposing (Note)


type Scale
    = MajorPentatonicScale Note
    | SuspendedPentatonicScale Note
    | BluesMinorPentatonicScale Note
    | BluesMajorPentatonicScale Note
    | MinorPentatonicScale Note
    | IonianScale Note
    | DorianScale Note
    | PhrygianScale Note
    | LydianScale Note
    | MyxolydianScale Note
    | AeolianScale Note
    | LocrianScale Note
    | ChromaticScale Note
    | FourthScale Note
    | FifthScale Note


toNotes : Scale -> List Note
toNotes scale =
    let getNotes tonic intervals = intervals |> Interval.structure tonic |> .notes in
    case scale of
        MajorPentatonicScale tonic ->
            [ MajorSecond, MajorSecond, MinorThird, MajorSecond, MinorThird ]
                |> getNotes tonic

        SuspendedPentatonicScale tonic ->
            [ MajorSecond, MinorThird, MajorSecond, MinorThird, MajorSecond ]
                |> getNotes tonic

        BluesMinorPentatonicScale tonic ->
            [ MinorThird, MajorSecond, MinorThird, MajorSecond, MajorSecond ]
                |> getNotes tonic

        BluesMajorPentatonicScale tonic ->
            [ MajorSecond, MinorThird, MajorSecond, MajorSecond, MinorThird ]
                |> getNotes tonic

        MinorPentatonicScale tonic ->
            [ MinorThird, MajorSecond, MajorSecond, MinorThird, MajorSecond ]
                |> getNotes tonic

        IonianScale tonic ->
            [ MajorSecond, MajorSecond, MinorSecond, MajorSecond, MajorSecond, MajorSecond, MinorSecond ]
                |> getNotes tonic

        DorianScale tonic ->
            [ MajorSecond, MinorSecond, MajorSecond, MajorSecond, MajorSecond, MinorSecond, MajorSecond ]
                |> getNotes tonic

        PhrygianScale tonic ->
            [ MinorSecond, MajorSecond, MajorSecond, MajorSecond, MinorSecond, MajorSecond, MajorSecond ]
                |> getNotes tonic

        LydianScale tonic ->
            [ MajorSecond, MajorSecond, MajorSecond, MinorSecond, MajorSecond, MajorSecond, MinorSecond ]
                |> getNotes tonic

        MyxolydianScale tonic ->
            [ MajorSecond, MajorSecond, MinorSecond, MajorSecond, MajorSecond, MinorSecond, MajorSecond ]
                |> getNotes tonic

        AeolianScale tonic ->
            [ MajorSecond, MinorSecond, MajorSecond, MajorSecond, MinorSecond, MajorSecond, MajorSecond ]
                |> getNotes tonic

        LocrianScale tonic ->
            [ MinorSecond, MajorSecond, MajorSecond, MinorSecond, MajorSecond, MajorSecond, MajorSecond ]
                |> getNotes tonic

        ChromaticScale tonic ->
            [ MinorSecond, MinorSecond, MinorSecond, MinorSecond, MinorSecond, MinorSecond
            , MinorSecond, MinorSecond, MinorSecond, MinorSecond, MinorSecond, MinorSecond
            ]
                |> getNotes tonic

        FourthScale tonic ->
            [ PerfectFourth, PerfectFourth, PerfectFourth, PerfectFourth, PerfectFourth, PerfectFourth
            , PerfectFourth, PerfectFourth, PerfectFourth, PerfectFourth, PerfectFourth, PerfectFourth
            ]
                |> getNotes tonic

        FifthScale tonic ->
            [ PerfectFifth, PerfectFifth, PerfectFifth, PerfectFifth, PerfectFifth, PerfectFifth
            , PerfectFifth, PerfectFifth, PerfectFifth, PerfectFifth, PerfectFifth, PerfectFifth
            ]
                |> getNotes tonic


toKindString : Scale -> String
toKindString scale =
    case scale of
        MajorPentatonicScale _ ->
            "major pentatonic"

        SuspendedPentatonicScale _ ->
            "suspended pentatonic"

        BluesMinorPentatonicScale _ ->
            "blues minor pentatonic"

        BluesMajorPentatonicScale _ ->
            "blues major pentatonic"

        MinorPentatonicScale _ ->
            "minor pentatonic"

        IonianScale _ ->
            "ionian"

        DorianScale _ ->
            "dorian"

        PhrygianScale _ ->
            "phrygian"

        LydianScale _ ->
            "lydian"

        MyxolydianScale _ ->
            "myxolydian"

        AeolianScale _ ->
            "aeolian"

        LocrianScale _ ->
            "locrian"

        ChromaticScale _ ->
            "chromatic"

        FourthScale _ ->
            "fourth circle"

        FifthScale _ ->
            "fifth circle"


fromKindString : String -> Maybe (Note -> Scale)
fromKindString val =
    case val of
        "major pentatonic" ->
            Just MajorPentatonicScale

        "suspended pentatonic" ->
            Just SuspendedPentatonicScale

        "blues minor pentatonic" ->
            Just BluesMinorPentatonicScale

        "blues major pentatonic" ->
            Just BluesMajorPentatonicScale

        "minor pentatonic" ->
            Just MinorPentatonicScale

        "ionian" ->
            Just IonianScale

        "dorian" ->
            Just DorianScale

        "phrygian" ->
            Just PhrygianScale

        "lydian" ->
            Just LydianScale

        "myxolydian" ->
            Just MyxolydianScale

        "aeolian" ->
            Just AeolianScale

        "locrian" ->
            Just LocrianScale

        "chromatic" ->
            Just ChromaticScale

        "fourth circle" ->
            Just FourthScale

        "fifth circle" ->
            Just FifthScale

        _ ->
            Nothing