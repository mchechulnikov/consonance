module Music exposing (..)


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


type Note
    = Note Base (Maybe Accidental) Octave


noteToString : Note -> String
noteToString note =
    let
        (Note base accidental octave) = note

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