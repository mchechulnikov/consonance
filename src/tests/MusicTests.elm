module MusicTests exposing (..)

import Expect exposing (Expectation)
import Note exposing (Accidental(..), Base(..), Note(..), Octave(..), toString)
import Test exposing (..)


noteToStringTests : Test
noteToStringTests =
    let
        testNote str val =
            test str (\_ -> Expect.equal str (toString val))
    in
    describe "note to string"
        [ testNote "A0" (Note A Nothing SubContraOctave)
        , testNote "B♯1" (Note B (Just Sharp) ContraOctave)
        , testNote "C♭2" (Note C (Just Flat) GreatOctave)
        , testNote "D♯3" (Note D (Just Sharp) SmallOctave)
        , testNote "E♭4" (Note E (Just Flat) OneLinedOctave)
        , testNote "F♯5" (Note F (Just Sharp) TwoLinedOctave)
        , testNote "G♭6" (Note G (Just Flat) ThreeLinedOctave)
        , testNote "A♯7" (Note A (Just Sharp) FourLinedOctave)
        , testNote "B♭8" (Note B (Just Flat) FiveLinedOctave)
        , testNote "C9" (Note C Nothing SixLinedOctave)
        ]
