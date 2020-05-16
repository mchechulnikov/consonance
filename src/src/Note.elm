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
    = SubContraOctave
    | ContraOctave
    | GreatOctave
    | SmallOctave
    | OneLinedOctave
    | TwoLinedOctave
    | ThreeLinedOctave
    | FourLinedOctave
    | FiveLinedOctave
    | SixLinedOctave


type alias Note =
    { base : Base
    , acc : Maybe Accidental
    , oct : Octave
    }


getNext : Note -> Note
getNext note =
    let
        { base, acc, oct } = note

        (nextBase, nextAccidental) =
            case (base, acc) of
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
                case oct of
                    SubContraOctave ->
                      ContraOctave

                    ContraOctave ->
                        GreatOctave

                    GreatOctave ->
                        SmallOctave

                    SmallOctave ->
                        OneLinedOctave

                    OneLinedOctave ->
                        TwoLinedOctave

                    TwoLinedOctave ->
                        ThreeLinedOctave

                    ThreeLinedOctave ->
                        FourLinedOctave

                    FourLinedOctave ->
                        FiveLinedOctave

                    FiveLinedOctave ->
                        SixLinedOctave

                    SixLinedOctave ->
                        SixLinedOctave
            else
                oct
    in
    { base = nextBase, acc = nextAccidental, oct = nextOctave }


plusSemitones : Int -> Note -> Note
plusSemitones semitonesNumber note =
    let
        gen prev n =
            if n > 0 then
                gen (getNext prev) (n - 1)

            else
                prev
    in
    gen note semitonesNumber


toString : Note -> String
toString note =
    let
        { base, acc, oct } = note

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
            acc
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
            case oct of
                SubContraOctave ->
                    0

                ContraOctave ->
                    1

                GreatOctave ->
                    2

                SmallOctave ->
                    3

                OneLinedOctave ->
                    4

                TwoLinedOctave ->
                    5

                ThreeLinedOctave ->
                    6

                FourLinedOctave ->
                    7

                FiveLinedOctave ->
                    8

                SixLinedOctave ->
                    9
    in
    baseString ++ accidentalString ++ (String.fromInt octaveNumber)