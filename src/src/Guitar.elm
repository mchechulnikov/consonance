module Guitar exposing (..)

import Chord exposing (Chord)
import Interval exposing (Interval)
import Note exposing (Base(..), Note, Octave(..))
import List.Extra as List exposing (..)
import Scale exposing (Scale)
import Utils exposing (boolToMaybe, flatten2D)



standardTuning
    = SixStringTuning
        { base = E, acc = Nothing, oct = OneLinedOctave }
        { base = B, acc = Nothing, oct = SmallOctave }
        { base = G, acc = Nothing, oct = SmallOctave }
        { base = D, acc = Nothing, oct = SmallOctave }
        { base = A, acc = Nothing, oct = GreatOctave }
        { base = E, acc = Nothing, oct = GreatOctave }



type Tuning
    = SixStringTuning Note Note Note Note Note Note
    | SevenStringTuning Note Note Note Note Note Note Note
    | EightStringTuning Note Note Note Note Note Note Note Note


type alias Guitar =
    { tuning : Tuning
    , fretsNumber : Int
    , layout : List (List Note)
    }


type GuitarString
    = GuitarString Int


type GuitarFret
    = GuitarFret Int


type alias FretPoint =
    { string : GuitarString
    , fret : GuitarFret
    , kind : FretPointKind
    }


type FretPointKind
    = NormalFretPoint
    | StartFretPoint
    | SelectedFretPoint
    | StopFretPoint


fretPoint : Guitar -> (FretPointKind, Int, Int) -> FretPoint
fretPoint guitar (kind, string, fret) =
    { string = guitarString guitar.tuning string
    , fret = guitarFret guitar.fretsNumber fret
    , kind = kind
    }


new : Tuning -> Int -> Guitar
new tuning fretNumbers =
    unwindTuning tuning
        |> List.map (Note.generateList fretNumbers)
        |> Guitar tuning fretNumbers


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


unwindTuning : Tuning -> List Note
unwindTuning tuning =
    case tuning of
        SixStringTuning n1 n2 n3 n4 n5 n6 ->
            [ n1, n2, n3, n4, n5, n6 ]

        SevenStringTuning n1 n2 n3 n4 n5 n6 n7 ->
            [ n1, n2, n3, n4, n5, n6, n7 ]

        EightStringTuning n1 n2 n3 n4 n5 n6 n7 n8 ->
            [ n1, n2, n3, n4, n5, n6, n7, n8 ]


takeChord : Guitar -> Chord -> List FretPoint
takeChord guitar chord =
    chord
        |> Chord.toNotes
        |> takeNotes guitar


takeScale : Guitar -> Scale -> List FretPoint
takeScale guitar scale =
    scale
        |> Scale.toNotes
        |> takeNotes guitar


takeInterval : Guitar -> Interval -> List FretPoint
takeInterval guitar interval =
    interval
        |> Interval.toNotes
        |> takeNotes guitar


takeNotes : Guitar -> List Note -> List FretPoint
takeNotes guitar notes =
    let
        firstNote = List.head notes
        lastNote = List.last notes

        calcFretPointType note =
            let noteIs n = n |> Maybe.map ((==) note) |> Maybe.withDefault False in
            if noteIs firstNote then
                StartFretPoint

            else if noteIs lastNote then
                StopFretPoint

            else
                SelectedFretPoint
    in
    guitar.layout
        |> List.indexedMap
            (\stringIndex stringLayout ->
                let
                    getStringFretPair fretNumber note =
                        List.member note notes
                            |> boolToMaybe (calcFretPointType note, stringIndex + 1, fretNumber)
                in
                stringLayout |> List.indexedMap getStringFretPair
            )
        |> flatten2D
        |> List.filter ((/=) Nothing)
        |> List.map (Maybe.withDefault (NormalFretPoint, 0, 0) >> fretPoint guitar)
