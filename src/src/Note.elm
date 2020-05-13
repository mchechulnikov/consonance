module Note exposing (..)


type Base
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type Accidental
    = Flat
    | Sharp


type Octave
    = SubContra
    | Contra
    | Great
    | Small
    | OneLined
    | TwoLined
    | ThreeLined
    | FourLined
    | FiveLined
    | SixLined


type alias Note =
    { base : Base
    , accidental : Maybe Accidental
    , octave : Octave
    }


getNext : Note -> Note
getNext note =
    let
        { base, accidental, octave } = note

        (nextBase, nextAccidental) =
            case (base, accidental) of
                (A, Just Flat) ->
                    (A, Nothing)

                (A, Nothing) ->
                    (A, Just Sharp)

                (A, Just Sharp) ->
                    (B, Nothing)

                (B, Just Flat) ->
                    (B, Nothing)

                (B, Nothing) ->
                    (C, Nothing)

                (B, Just Sharp) ->
                    (C, Just Sharp)

                (C, Just Flat) ->
                    (C, Nothing)

                (C, Nothing) ->
                    (C, Just Sharp)

                (C, Just Sharp) ->
                    (D, Nothing)

                (D, Just Flat) ->
                    (D, Nothing)

                (D, Nothing) ->
                    (D, Just Sharp)

                (D, Just Sharp) ->
                    (E, Nothing)

                (E, Just Flat) ->
                    (E, Nothing)

                (E, Nothing) ->
                    (F, Nothing)

                (E, Just Sharp) ->
                    (F, Just Sharp)

                (F, Just Flat) ->
                    (F, Nothing)

                (F, Nothing) ->
                    (F, Just Sharp)

                (F, Just Sharp) ->
                    (G, Nothing)

                (G, Just Flat) ->
                    (G, Nothing)

                (G, Nothing) ->
                    (G, Just Sharp)

                (G, Just Sharp) ->
                    (A, Nothing)

        nextOctave =
            if base == B then
                case octave of
                    SubContra ->
                      Contra

                    Contra ->
                        Great

                    Great ->
                        Small

                    Small ->
                        OneLined

                    OneLined ->
                        TwoLined

                    TwoLined ->
                        ThreeLined

                    ThreeLined ->
                        FourLined

                    FourLined ->
                        FiveLined

                    FiveLined ->
                        SixLined

                    SixLined ->
                        SixLined
            else
                octave
    in
    { base = nextBase, accidental = nextAccidental, octave = nextOctave }


toString : Note -> String
toString note =
    let
        { base, accidental, octave } = note

        baseString =
            case base of
                A ->
                    "A"

                B ->
                    "B"

                C ->
                    "C"

                D ->
                    "D"

                E ->
                    "E"

                F ->
                    "F"

                G ->
                    "G"

        accidentalString =
            accidental
                |> Maybe.map
                    (\a ->
                        case a of
                            Flat ->
                                "♭"

                            Sharp ->
                                "♯"
                    )
                |> Maybe.withDefault ""

        octaveNumber=
            case octave of
                SubContra ->
                    0

                Contra ->
                    1

                Great ->
                    2

                Small ->
                    3

                OneLined ->
                    4

                TwoLined ->
                    5

                ThreeLined ->
                    6

                FourLined ->
                    7

                FiveLined ->
                    8

                SixLined ->
                    9
    in
    baseString ++ accidentalString ++ (String.fromInt octaveNumber)