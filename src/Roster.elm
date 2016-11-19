module Roster
    exposing
        ( Roster
        , initialize
        , append
        , prepend
        , scrollUp
        , scrollDown
        , isEmpty
        , isFull
        , isFirstPositionEmpty
        , isLastPositionEmpty
        , toList
        , toElementsList
        , first
        , last
        )

import Array.Hamt as Array


type alias Roster a =
    { size : Int
    , elements : Array.Array a
    , offset :
        -- Index of the roster at which the first element starts.
        Int
    }


initialize : Int -> Roster a
initialize size =
    let
        normalizedSize =
            max 0 size
    in
        { size = normalizedSize
        , elements = Array.empty
        , offset = 0
        }



-- Array helpers


arrayLast : Array.Array a -> Maybe a
arrayLast array =
    let
        arrayLength =
            Array.length array
    in
        Array.get (arrayLength - 1) array


arrayTail : Array.Array a -> Array.Array a
arrayTail array =
    Array.slice 1 ((Array.length array) - 1) array



-- Helpers


isEmpty : Roster a -> Bool
isEmpty roster =
    Array.isEmpty roster.elements


isFull : Roster a -> Bool
isFull roster =
    Array.length roster.elements == roster.size


toList : Roster a -> List (Maybe a)
toList roster =
    let
        nToPrepend =
            roster.offset

        nToAppend =
            roster.size - nToPrepend - (Array.length roster.elements)

        justElements =
            Array.map Just roster.elements
    in
        List.concat
            [ (List.repeat nToPrepend Nothing)
            , (Array.toList justElements)
            , (List.repeat nToAppend Nothing)
            ]


toElementsList : Roster a -> List a
toElementsList roster =
    Array.toList roster.elements


first : Roster a -> Maybe a
first roster =
    Array.get 0 roster.elements


last : Roster a -> Maybe a
last =
    .elements >> arrayLast


isFirstPositionEmpty : Roster a -> Bool
isFirstPositionEmpty roster =
    (roster.offset > 0) || (isEmpty roster)


{-| To better understand this function, here are some example rosters. `a` represents
a value of type `a` of `Roster a`.

0               |   |   | a | a |   |   |   |   |   |
1               | a |   | a | a | a | a |   |   |   |
2               | a | a | a | a | a | a |   |   |   |
3               |   | a | a | a | a | a |   | a |   |
4               |   | a |   | a | a |   |   |   | a |
----------------|---|---|---|---|---|---|---|---|---|
Offset:         | 1 | 2 | 0 | 0 | 1 | 1 | 0 | 3 | 4 |
No. of elements:| 2 | 3 | 4 | 5 | 4 | 3 | 5 | 1 | 1 |
Roster size:    | 5 | 5 | 5 | 5 | 5 | 5 | 5 | 5 | 5 |

The equation below is false for all rosters with the last position occupied by an element.
-}
isLastPositionEmpty : Roster a -> Bool
isLastPositionEmpty roster =
    (roster.offset + (Array.length roster.elements)) < roster.size



-- Offset functions


lowerOffsetBound : Int
lowerOffsetBound =
    0


ensureOffsetBounds : Roster a -> Int -> Int
ensureOffsetBounds roster newOffset =
    let
        upperOffsetBound =
            roster.size - 1
    in
        newOffset |> min upperOffsetBound |> max lowerOffsetBound


increaseOffset : Roster a -> Roster a
increaseOffset roster =
    { roster | offset = ensureOffsetBounds roster (roster.offset + 1) }


decreaseOffset : Roster a -> Roster a
decreaseOffset roster =
    { roster | offset = ensureOffsetBounds roster (roster.offset - 1) }



-- Prepend


prependElement : a -> Roster a -> Roster a
prependElement element roster =
    { roster | elements = Array.append (Array.repeat 1 element) roster.elements }


decreaseOffsetIfElementsPresent : Roster a -> Roster a
decreaseOffsetIfElementsPresent roster =
    if (Array.isEmpty roster.elements) then
        roster
    else
        decreaseOffset roster


prepend : a -> Roster a -> Maybe (Roster a)
prepend newElement roster =
    case isFirstPositionEmpty roster of
        True ->
            roster
                |> decreaseOffsetIfElementsPresent
                |> prependElement newElement
                |> Just

        False ->
            Nothing



-- Append


append : a -> Roster a -> Maybe (Roster a)
append newElement roster =
    if isLastPositionEmpty roster then
        -- Appending an element to the end doesn't change the offset.
        Just { roster | elements = Array.push newElement roster.elements }
    else
        Nothing



-- Scrolling


dropElementOnLastPositionIfAny : Roster a -> Roster a
dropElementOnLastPositionIfAny roster =
    if isLastPositionEmpty roster then
        roster
    else
        let
            -- We need to exclude the last element of the array.
            indexOfOneBeforeLastElement =
                (Array.length roster.elements) - 2
        in
            { roster | elements = Array.slice 0 indexOfOneBeforeLastElement roster.elements }


scrollUp : Roster a -> Roster a
scrollUp =
    dropElementOnLastPositionIfAny >> increaseOffset


dropElementOnFirstPositionIfAny : Roster a -> Roster a
dropElementOnFirstPositionIfAny roster =
    if roster.offset == 0 && (not <| Array.isEmpty roster.elements) then
        { roster | elements = arrayTail roster.elements }
    else
        roster


scrollDown : Roster a -> Roster a
scrollDown =
    dropElementOnFirstPositionIfAny >> decreaseOffset
