module MusicTests exposing (..)

import Expect exposing (Expectation)
import Music exposing (Accidental(..), Base(..), Note(..), Octave(..), noteToString)
import Test exposing (..)


noteToStringTests : Test
noteToStringTests =
    let
        testNote str val =
            test str (\_ -> Expect.equal str (noteToString val))
    in
    describe "note to string"
        [ testNote "A0" (Note A Nothing SubContra)
        , testNote "B♯1" (Note B (Just Sharp) Contra)
        , testNote "C♭2" (Note C (Just Flat) Great)
        , testNote "D♯3" (Note D (Just Sharp) Small)
        , testNote "E♭4" (Note E (Just Flat) OneLined)
        , testNote "F♯5" (Note F (Just Sharp) TwoLined)
        , testNote "G♭6" (Note G (Just Flat) ThreeLined)
        , testNote "A♯7" (Note A (Just Sharp) FourLined)
        , testNote "B♭8" (Note B (Just Flat) FiveLined)
        , testNote "C9" (Note C Nothing SixLined)
        ]
