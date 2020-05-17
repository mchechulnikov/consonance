module ScaleTests exposing (..)

import Expect exposing (Expectation)
import Note exposing (Base(..), Octave(..))
import Scale exposing (..)
import Test exposing (..)


toKindStringTests : Test
toKindStringTests =
    let
        testNote val str  =
            test str (\_ -> Expect.equal str (Scale.toKindString val))
    in
    describe "scale to scale kind string"
        [ testNote (MajorPentatonicScale { base = A, acc = Nothing, oct = SmallOctave }) "major pentatonic"
        , testNote (MinorPentatonicScale { base = A, acc = Nothing, oct = SmallOctave }) "minor pentatonic"
        ]


fromKindStringTests : Test
fromKindStringTests =
    let
        testNote str val =
            test str (\_ -> Expect.equal val (Scale.fromKindString str))
    in
    describe "scale kind from string"
        [ testNote "major pentatonic" (Just MajorPentatonicScale)
        , testNote "minor pentatonic" (Just MinorPentatonicScale)
        ]