module Note exposing (..)

import Parser exposing ((|.), (|=), backtrackable, deadEndsToString, end, int, keyword, oneOf, succeed, symbol)



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


generateList : Int -> Note -> List Note
generateList amount firstNote =
    let
        gen : Note -> Int -> List Note -> List Note
        gen prev n acc =
            if n > 0 then
                let next = getNext prev in
                gen next (n - 1) (next :: acc)

            else
                acc
    in
    gen firstNote amount [ firstNote ]
        |> List.reverse


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


fromString : String -> Maybe Note
fromString val =
    let
        baseParser =
            oneOf
                [ succeed A |. symbol "A"
                , succeed B |. symbol "B"
                , succeed C |. symbol "C"
                , succeed D |. symbol "D"
                , succeed E |. symbol "E"
                , succeed F |. symbol "F"
                , succeed G |. symbol "G"
                ]

        accParser =
            oneOf
                [ succeed (Just Sharp) |. symbol "♯"
                , succeed (Just Flat) |. symbol  "♭"
                , succeed Nothing
                ]

        octParser =
            oneOf
                [ succeed SubContraOctave |. symbol "0"
                , succeed ContraOctave |. symbol "1"
                , succeed GreatOctave |. symbol "2"
                , succeed SmallOctave |. symbol "3"
                , succeed OneLinedOctave |. symbol "4"
                , succeed TwoLinedOctave |. symbol "5"
                , succeed ThreeLinedOctave |. symbol "6"
                , succeed FourLinedOctave |. symbol "7"
                , succeed FiveLinedOctave |. symbol "8"
                , succeed SixLinedOctave |. symbol "9"
                ]
    in
    Parser.run (succeed Note |= baseParser |= accParser |= octParser |. end) val
        |> Result.toMaybe
