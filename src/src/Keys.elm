module Keys exposing (..)

import Chord exposing (Chord)
import Interval exposing (Interval)
import List.Extra as List
import Note exposing (Base(..), Note, Octave(..))
import Scale exposing (Scale)

type alias Key =
    { note : Note
    , kind : KeyKind
    , color : KeyColor
    }

type KeyKind
    = NormalKey
    | StartKey
    | SelectedKey
    | StopKey

type KeyColor
    = WhiteKey
    | BlackKey

keysNumber =
    88


firstKeyNote : Note
firstKeyNote =
    { base = A, acc = Nothing, oct = SubContraOctave }


layout : List Key
layout =
    Note.generateList (keysNumber - 1) firstKeyNote
        |> List.map (\note -> { note = note, kind = NormalKey, color = getKeyColor note })


getKeyColor : Note -> KeyColor
getKeyColor note =
    if note.acc == Nothing then
        WhiteKey

    else
        BlackKey


takeNotes : List Note -> List Key
takeNotes notes =
    let
        firstNote = List.head notes
        lastNote = List.last notes

        calcKind note =
            let noteIs n = n |> Maybe.map ((==) note) |> Maybe.withDefault False in
            if noteIs firstNote then
                StartKey

            else if noteIs lastNote then
                StopKey

            else if notes |> List.member note then
                SelectedKey

            else
                NormalKey
    in
    layout |> List.map (\key -> { key | kind = calcKind key.note })


takeChord : Chord -> List Key
takeChord chord =
    chord
        |> Chord.toNotes
        |> takeNotes


takeScale : Scale -> List Key
takeScale scale =
    scale
        |> Scale.toNotes
        |> takeNotes


takeInterval : Interval -> List Key
takeInterval interval =
    interval
        |> Interval.toNotes
        |> takeNotes

