module CustomFuzzers exposing (rosterScrollAction)

import Fuzz
import Random.Pcg as Random
import Shrink


-- Our imports

import Actions exposing (RosterScrollAction(..))


rosterScrollAction : Fuzz.Fuzzer RosterScrollAction
rosterScrollAction =
    let
        generator =
            Random.choice ScrollUp ScrollDown

        shrinker =
            Shrink.noShrink
    in
        Fuzz.custom generator shrinker
