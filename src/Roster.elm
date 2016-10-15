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
        , toList
        , first
        , last
        )


type alias Roster a =
    { size : Int
    , elements :
        -- In an ideal world we'd use a data structure that can efficiently add new elements
        -- to both sides and that can return its size in constant time. Since the actuall app
        -- is going to store no more than five elements, Array would be a good candidate
        -- despite linear prepending time.
        --
        -- However, the Elm Array implementation is quite buggy [1], so instead of fighting
        -- with it, let's use a List for the time being.
        -- [1]: https://github.com/elm-lang/core/issues/649
        List a
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
        , elements = []
        , offset = 0
        }



-- Helpers


isEmpty : Roster a -> Bool
isEmpty roster =
    List.isEmpty roster.elements


isFull : Roster a -> Bool
isFull roster =
    List.length roster.elements == roster.size


toList : Roster a -> List (Maybe a)
toList roster =
    let
        nToPrepend =
            roster.offset

        nToAppend =
            roster.size - nToPrepend - (List.length roster.elements)

        justElements =
            List.map Just roster.elements
    in
        List.concat
            [ (List.repeat nToPrepend Nothing)
            , justElements
            , (List.repeat nToAppend Nothing)
            ]


first : Roster a -> Maybe a
first roster =
    List.head roster.elements


listLast : List a -> Maybe a
listLast list =
    case list of
        head :: [] ->
            Just head

        head :: tail ->
            listLast tail

        [] ->
            Nothing


last : Roster a -> Maybe a
last roster =
    listLast roster.elements



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


isFirstPositionEmpty : Roster a -> Bool
isFirstPositionEmpty roster =
    (roster.offset > 0) || (isEmpty roster)


prependElement : a -> Roster a -> Roster a
prependElement element roster =
    { roster | elements = element :: roster.elements }


decreaseOffsetIfElementsPresent : Roster a -> Roster a
decreaseOffsetIfElementsPresent roster =
    if (List.isEmpty roster.elements) then
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
    (roster.offset + (List.length roster.elements)) < roster.size


append : a -> Roster a -> Maybe (Roster a)
append newElement roster =
    if isLastPositionEmpty roster then
        -- Appending an element to the end doesn't change the offset.
        Just { roster | elements = List.append roster.elements [ newElement ] }
    else
        Nothing



-- Scrolling


dropElementOnLastPositionIfAny : Roster a -> Roster a
dropElementOnLastPositionIfAny roster =
    if isLastPositionEmpty roster then
        roster
    else
        let
            nOfElementsToTake =
                (List.length roster.elements) - 1
        in
            { roster | elements = List.take nOfElementsToTake roster.elements }


scrollUp : Roster a -> Roster a
scrollUp =
    dropElementOnLastPositionIfAny >> increaseOffset


dropElementOnFirstPositionIfAny : Roster a -> Roster a
dropElementOnFirstPositionIfAny roster =
    if roster.offset == 0 && (not <| List.isEmpty roster.elements) then
        { roster | elements = List.tail roster.elements |> Maybe.withDefault [] }
    else
        roster


scrollDown : Roster a -> Roster a
scrollDown =
    dropElementOnFirstPositionIfAny >> decreaseOffset
