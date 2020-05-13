module Guitar exposing (..)

import Note exposing (Base(..), Note, Octave(..))



standardTuning
    = SixStringTuning
        { base = E, accidental = Nothing, octave = OneLined}
        { base = B, accidental = Nothing, octave = Small}
        { base = G, accidental = Nothing, octave = Small}
        { base = D, accidental = Nothing, octave = Small}
        { base = A, accidental = Nothing, octave = Great}
        { base = E, accidental = Nothing, octave = Great}



type Tuning
    = SixStringTuning Note Note Note Note Note Note
    | SevenStringTuning Note Note Note Note Note Note Note
    | EightStringTuning Note Note Note Note Note Note Note Note


type alias Guitar =
    { tuning : Tuning
    , fretsNumber : Int
    }


type GuitarString
    = GuitarString Int


type GuitarFret
    = GuitarFret Int


type alias FretPoint =
    { string : GuitarString
    , fret : GuitarFret
    }


fretPoint : Guitar -> (Int, Int) -> FretPoint
fretPoint guitar (string, fret) =
    { string = guitarString guitar.tuning string
    , fret = guitarFret guitar.fretsNumber fret
    }


guitarString : Tuning -> Int -> GuitarString
guitarString tuning value =
    let clampString bound = clamp 0 bound value in
    case tuning of
        SixStringTuning _ _ _ _ _ _ ->
            GuitarString (clampString 6)

        SevenStringTuning _ _ _ _ _ _ _ ->
            GuitarString (clampString 7)

        EightStringTuning _ _ _ _ _ _ _ _ ->
            GuitarString (clampString 8)


guitarFret : Int -> Int -> GuitarFret
guitarFret fretsNumber value =
    let clampFret bound = clamp 0 bound value in
    GuitarFret (clampFret fretsNumber)


generateStringLayout : Int -> Note -> List Note
generateStringLayout fretNumbers stringTuning =
    let
        gen : Note -> Int -> List Note -> List Note
        gen prev n acc =
            if n > 0 then
                let next = Note.getNext prev in
                gen next (n - 1) (next :: acc)

            else
                acc
    in
    gen stringTuning fretNumbers [ stringTuning ]
        |> List.reverse


unwindTuning : Tuning -> List Note
unwindTuning tuning =
    case tuning of
        SixStringTuning n1 n2 n3 n4 n5 n6 ->
            [ n1, n2, n3, n4, n5, n6 ]

        SevenStringTuning n1 n2 n3 n4 n5 n6 n7 ->
            [ n1, n2, n3, n4, n5, n6, n7 ]

        EightStringTuning n1 n2 n3 n4 n5 n6 n7 n8 ->
            [ n1, n2, n3, n4, n5, n6, n7, n8 ]
