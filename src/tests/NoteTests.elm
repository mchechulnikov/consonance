module NoteTests exposing (..)

import Expect exposing (Expectation)
import Note exposing (Accidental(..), Base(..), Octave(..))
import Test exposing (..)


toStringTests : Test
toStringTests =
    let
        testNote val str  =
            test str (\_ -> Expect.equal str (Note.toString val))
    in
    describe "note to string"
        [ testNote { base = A, acc = Nothing, oct = SubContraOctave } "A0"
        , testNote { base = B, acc = Just Sharp, oct = ContraOctave } "B♯1"
        , testNote { base = C, acc = Just Flat, oct = GreatOctave } "C♭2"
        , testNote { base = D, acc = Just Sharp, oct = SmallOctave } "D♯3"
        , testNote { base = E, acc = Just Flat, oct = OneLinedOctave } "E♭4"
        , testNote { base = F, acc = Just Sharp, oct = TwoLinedOctave } "F♯5"
        , testNote { base = G, acc = Just Flat, oct = ThreeLinedOctave } "G♭6"
        , testNote { base = A, acc = Just Sharp, oct = FourLinedOctave } "A♯7"
        , testNote { base = B, acc = Just Flat, oct = FiveLinedOctave } "B♭8"
        , testNote { base = C, acc = Nothing, oct = SixLinedOctave } "C9"
        ]


fromStringTests : Test
fromStringTests =
    let
        testNote str val =
            test str (\_ -> Expect.equal val (Note.fromString str))
    in
    describe "note from string"
        [ testNote "A0" (Just { base = A, acc = Nothing, oct = SubContraOctave })
        , testNote "B♯1" (Just { base = B, acc = Just Sharp, oct = ContraOctave })
        , testNote "C♭2" (Just { base = C, acc = Just Flat, oct = GreatOctave })
        , testNote "D♯3" (Just { base = D, acc = Just Sharp, oct = SmallOctave })
        , testNote "E♭4" (Just { base = E, acc = Just Flat, oct = OneLinedOctave })
        , testNote "F♯5" (Just { base = F, acc = Just Sharp, oct = TwoLinedOctave })
        , testNote "G♭6" (Just { base = G, acc = Just Flat, oct = ThreeLinedOctave })
        , testNote "A♯7" (Just { base = A, acc = Just Sharp, oct = FourLinedOctave })
        , testNote "B♭8" (Just { base = B, acc = Just Flat, oct = FiveLinedOctave })
        , testNote "C9" (Just { base = C, acc = Nothing, oct = SixLinedOctave })
        ]