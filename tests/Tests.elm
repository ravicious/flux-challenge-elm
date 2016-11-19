module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz


-- Our imports

import Utils exposing (applyNTimes)
import Roster
import Actions exposing (applyRosterScrollAction)
import CustomFuzzers


rosterTests : Test
rosterTests =
    describe "Tests for the Roster data structure"
        [ fuzz2
            (Fuzz.list (Fuzz.intRange 0 50))
            (Fuzz.intRange 0 50)
            "Appending n + 1 elements to a roster of size n returns Nothing"
          <|
            \nElements extraElement ->
                let
                    roster =
                        Roster.initialize (List.length nElements)

                    elementsToAppend =
                        extraElement :: nElements

                    result =
                        elementsToAppend
                            |> List.foldl
                                (\element roster ->
                                    roster |> Maybe.andThen (Roster.append element)
                                )
                                (Just roster)
                in
                    case result of
                        Just roster ->
                            Expect.fail "Expected the result to be Nothing"

                        Nothing ->
                            Expect.pass
        , fuzz3
            (Fuzz.char)
            (Fuzz.list Fuzz.char)
            (Fuzz.intRange 1 50)
            "Scrolling up n - 1 times and prepending n elements to roster of size k (k >= n, n > 0) is the same as reversing n elements and appending them"
          <|
            \element rawElements rosterSize ->
                let
                    roster =
                        Roster.initialize rosterSize

                    -- Make sure we always have at leas one element (n > 0)
                    -- and there's at most k elements (k >= n).
                    elements =
                        element :: rawElements |> List.take rosterSize

                    scrollTimes =
                        (List.length elements) - 1

                    scrollUpThenPrepend =
                        (applyNTimes scrollTimes Roster.scrollUp)
                            >> Just
                            >> \roster ->
                                List.foldl
                                    (\element roster ->
                                        roster |> Maybe.andThen (Roster.prepend element)
                                    )
                                    roster
                                    elements

                    append =
                        Just
                            >> \roster ->
                                List.foldl
                                    (\element roster ->
                                        roster |> Maybe.andThen (Roster.append element)
                                    )
                                    roster
                                    (List.reverse elements)
                in
                    case ( (scrollUpThenPrepend roster), (append roster) ) of
                        ( Just prependRoster, Just appendRoster ) ->
                            Expect.equal (Roster.toList prependRoster) (Roster.toList appendRoster)

                        ( Nothing, _ ) ->
                            Expect.fail "Prepending the roster returned Nothing"

                        ( _, Nothing ) ->
                            Expect.fail "Appending the roster returned Nothing"
        , fuzz2
            (Fuzz.list CustomFuzzers.rosterScrollAction)
            (Fuzz.intRange 0 50)
            "Scrolling an empty roster has no effect on the toList output"
          <|
            \scrollActions rosterSize ->
                let
                    roster =
                        Roster.initialize rosterSize

                    expected =
                        Roster.toList roster

                    actual =
                        List.foldl applyRosterScrollAction roster scrollActions |> Roster.toList
                in
                    Expect.equal expected actual
        ]


all : Test
all =
    Test.concat [ rosterTests ]
