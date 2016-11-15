module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import WebSocket
import Json.Decode as Json exposing (field)
import Http
import Set exposing (Set)


-- Our imports

import Roster exposing (Roster)
import Utils


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


darkJedisRosterSize : Int
darkJedisRosterSize =
    5


type alias Planet =
    { id : Int
    , name : String
    }


type alias DarkJediMetaData =
    { id : Int
    , url : String
    }


type alias DarkJediId =
    Int


type alias DarkJedi =
    { id : DarkJediId
    , name : String
    , homeworld : Planet
    , master : Maybe DarkJediMetaData
    , apprentice : Maybe DarkJediMetaData
    }


type alias DarkJediRoster =
    Roster DarkJedi


type alias Model =
    { currentPlanet : Maybe Planet
    , darkJedis : DarkJediRoster
    , darkJediIdsOnCurrentPlanet : Set DarkJediId
    }


isFrozen : Model -> Bool
isFrozen model =
    model.darkJediIdsOnCurrentPlanet |> (not << Set.isEmpty)


{-| According to flux-challenge docs, this is the first jedi we're supposed to fetch.
-}
firstDarkJediIdToFetch : Int
firstDarkJediIdToFetch =
    3616


init : ( Model, Cmd Msg )
init =
    ( { currentPlanet = Nothing
      , darkJedis = Roster.initialize darkJedisRosterSize
      , darkJediIdsOnCurrentPlanet = Set.empty
      }
    , fetchDarkJedi firstDarkJediIdToFetch
    )



-- Generic jedi helpers


{-| A generic function which allows us to compose a function which returns a jedi from a roster
and a function which returns a relative of a jedi. By composing these two functions, we can
construct a function which allows us to fetch the apprentice of the last jedi or the master
of the first jedi.
-}
relativeOfJedi :
    (DarkJediRoster -> Maybe DarkJedi)
    -> (DarkJedi -> Maybe DarkJediMetaData)
    -> DarkJediRoster
    -> Maybe DarkJediMetaData
relativeOfJedi fetchJedi fetchRelative =
    fetchJedi >> Maybe.andThen fetchRelative


apprenticeOfLastJedi : DarkJediRoster -> Maybe DarkJediMetaData
apprenticeOfLastJedi =
    relativeOfJedi Roster.last .apprentice


masterOfFirstJedi : DarkJediRoster -> Maybe DarkJediMetaData
masterOfFirstJedi =
    relativeOfJedi Roster.first .master


doesJediHaveRelativeJedi :
    (DarkJediRoster -> Maybe DarkJediMetaData)
    -> DarkJediRoster
    -> Bool
doesJediHaveRelativeJedi fetchRelatedJediMetaData roster =
    case fetchRelatedJediMetaData roster of
        Just _ ->
            True

        Nothing ->
            False


isRelativeOfJedi :
    (DarkJediRoster -> Maybe DarkJediMetaData)
    -> DarkJediRoster
    -> DarkJedi
    -> Bool
isRelativeOfJedi fetchRelativeMetaData roster jedi =
    case fetchRelativeMetaData roster of
        Just relative ->
            relative.id == jedi.id

        Nothing ->
            False


requestRelativeJediIfEnoughSpace :
    (DarkJediRoster -> Maybe DarkJediMetaData)
    -> (DarkJediRoster -> Bool)
    -> DarkJediRoster
    -> Cmd Msg
requestRelativeJediIfEnoughSpace fetchRelatedJediMetaData isThereSpaceForRelatedJedi roster =
    case fetchRelatedJediMetaData roster of
        Just jedi ->
            if isThereSpaceForRelatedJedi roster then
                fetchDarkJedi jedi.id
            else
                Cmd.none

        Nothing ->
            Cmd.none


requestRelativeJedisIfEnoughSpace : DarkJediRoster -> Cmd Msg
requestRelativeJedisIfEnoughSpace roster =
    let
        apprenticeCmd =
            -- The last position has to be empty in order to append an apprentice.
            requestRelativeJediIfEnoughSpace apprenticeOfLastJedi Roster.isLastPositionEmpty

        masterCmd =
            -- The first position has to be empty in order to prepend an apprentice.
            requestRelativeJediIfEnoughSpace masterOfFirstJedi Roster.isFirstPositionEmpty
    in
        [ apprenticeCmd, masterCmd ] |> List.map (\f -> f roster) |> Cmd.batch



-- Update


type Msg
    = VisitPlanet Planet
    | PlanetParsingError String
    | AddNewDarkJedi (Result Http.Error DarkJedi)
    | ScrollUp
    | ScrollDown


addJediToRoster : DarkJedi -> DarkJediRoster -> Maybe DarkJediRoster
addJediToRoster newJedi roster =
    if Roster.isFull roster then
        Nothing
    else if Roster.isEmpty roster then
        Roster.append newJedi roster
    else if isRelativeOfJedi apprenticeOfLastJedi roster newJedi then
        Roster.append newJedi roster
    else if isRelativeOfJedi masterOfFirstJedi roster newJedi then
        Roster.prepend newJedi roster
    else
        Nothing


lookForJedisOnPlanet : Planet -> List DarkJedi -> Set DarkJediId
lookForJedisOnPlanet planet darkJedis =
    List.foldl
        (\jedi jedisOnPlanet ->
            if jedi.homeworld.id == planet.id then
                Set.insert jedi.id jedisOnPlanet
            else
                jedisOnPlanet
        )
        Set.empty
        darkJedis


updateModelWithDarkJedisOnCurrentPlanet : Maybe Planet -> Model -> Model
updateModelWithDarkJedisOnCurrentPlanet planet model =
    case planet of
        Just planet ->
            { model
                | darkJediIdsOnCurrentPlanet =
                    lookForJedisOnPlanet planet
                        (Roster.toElementsList model.darkJedis)
            }

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VisitPlanet newPlanet ->
            let
                updatedModel =
                    { model
                        | currentPlanet = Just newPlanet
                    }
                        |> updateModelWithDarkJedisOnCurrentPlanet (Just newPlanet)

                cmd =
                    -- When the model is frozen, no new jedis are added to the roster.
                    -- Thus, if the updated model is not frozen, we have to make sure
                    -- to fetch new jedis in case any were dropped during the freeze.
                    if isFrozen model && (not <| isFrozen updatedModel) then
                        requestRelativeJedisIfEnoughSpace model.darkJedis
                    else
                        Cmd.none
            in
                ( updatedModel, cmd )

        ScrollUp ->
            let
                updatedModel =
                    { model | darkJedis = Utils.applyNTimes 2 Roster.scrollUp model.darkJedis }
            in
                ( updatedModel, requestRelativeJedisIfEnoughSpace updatedModel.darkJedis )

        ScrollDown ->
            let
                updatedModel =
                    { model | darkJedis = Utils.applyNTimes 2 Roster.scrollDown model.darkJedis }
            in
                ( updatedModel, requestRelativeJedisIfEnoughSpace updatedModel.darkJedis )

        AddNewDarkJedi (Ok newDarkJedi) ->
            -- If the model is frozen, we're supposed to cancel all ongoing requests.
            -- Since it's not straightforward to do in Elm,
            -- we're dropping any incoming jedis instead.
            if isFrozen model then
                ( model, Cmd.none )
            else
                let
                    updatedModel =
                        { model
                            | darkJedis =
                                -- If for some reason adding the jedi failed, use the existing roster.
                                addJediToRoster newDarkJedi model.darkJedis
                                    |> Maybe.withDefault model.darkJedis
                        }
                            |> updateModelWithDarkJedisOnCurrentPlanet model.currentPlanet

                    command =
                        requestRelativeJedisIfEnoughSpace updatedModel.darkJedis
                in
                    ( updatedModel, command )

        AddNewDarkJedi (Err _) ->
            ( model, Cmd.none )

        PlanetParsingError _ ->
            ( model, Cmd.none )



-- Decoders


planetDecoder : Json.Decoder Planet
planetDecoder =
    Json.map2 Planet (field "id" Json.int) (field "name" Json.string)


darkJediMetaDataDecoder : Json.Decoder DarkJediMetaData
darkJediMetaDataDecoder =
    Json.map2 DarkJediMetaData (field "id" Json.int) (field "url" Json.string)


darkJediDecoder : Json.Decoder DarkJedi
darkJediDecoder =
    Json.map5 DarkJedi
        (field "id" Json.int)
        (field "name" Json.string)
        (field "homeworld" planetDecoder)
        (Json.maybe (field "master" darkJediMetaDataDecoder))
        (Json.maybe (field "apprentice" darkJediMetaDataDecoder))



-- Effects


fetchDarkJedi : Int -> Cmd Msg
fetchDarkJedi id =
    let
        url =
            "http://localhost:3000/dark-jedis/" ++ (toString id)
    in
        Http.send AddNewDarkJedi <| Http.get url darkJediDecoder



-- Subscriptions


planetJsonToMsg : String -> Msg
planetJsonToMsg json =
    let
        decodeResult =
            Json.decodeString planetDecoder json
    in
        case decodeResult of
            Ok planet ->
                VisitPlanet planet

            Err error ->
                PlanetParsingError (Debug.log "Planet parsing error" error)


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.listen "ws://localhost:4000" planetJsonToMsg



-- View


planetToText : Maybe Planet -> String
planetToText =
    (Maybe.map .name) >> Maybe.withDefault ""


redTextStyle : Attribute msg
redTextStyle =
    style
        [ ( "color", "red" )
        ]


noStyle : Attribute msg
noStyle =
    style []


darkJediToListItem : Set DarkJediId -> Maybe DarkJedi -> Html Msg
darkJediToListItem darkJediIdsOnCurrentPlanet darkJedi =
    case darkJedi of
        Just darkJedi ->
            let
                isJediOnCurrentPlanet =
                    Set.member darkJedi.id darkJediIdsOnCurrentPlanet

                textStyle =
                    if isJediOnCurrentPlanet then
                        redTextStyle
                    else
                        noStyle
            in
                li [ class "css-slot" ]
                    [ h3 [ textStyle ] [ text darkJedi.name ]
                    , h6 [ textStyle ] [ text ("Homeworld: " ++ darkJedi.homeworld.name) ]
                    ]

        Nothing ->
            li [ class "css-slot" ] []


rosterToListItems : DarkJediRoster -> Set DarkJediId -> List (Html Msg)
rosterToListItems roster darkJediIdsOnCurrentPlanet =
    roster
        |> Roster.toList
        |> List.map (darkJediToListItem darkJediIdsOnCurrentPlanet)


view : Model -> Html Msg
view model =
    let
        isModelFrozen =
            isFrozen model

        -- If the boundary jedis have no relatives, we should disable the buttons.
        isScrollUpDisabled =
            isModelFrozen || (not <| doesJediHaveRelativeJedi masterOfFirstJedi model.darkJedis)

        isScrollDownDisabled =
            isModelFrozen || (not <| doesJediHaveRelativeJedi apprenticeOfLastJedi model.darkJedis)
    in
        div [ class "css-root" ]
            [ h1 [ class "css-planet-monitor" ]
                [ text ("Obi-Wan currently on " ++ (planetToText model.currentPlanet))
                ]
            , section [ class "css-scrollable-list" ]
                [ ul [ class "css-slots" ]
                    (rosterToListItems model.darkJedis
                        model.darkJediIdsOnCurrentPlanet
                    )
                , div [ class "css-scroll-buttons" ]
                    [ button
                        [ classList
                            [ ( "css-button-up", True )
                            , ( "css-button-disabled", isScrollUpDisabled )
                            ]
                        , onClick ScrollUp
                        , disabled isScrollUpDisabled
                        ]
                        []
                    , button
                        [ classList
                            [ ( "css-button-down", True )
                            , ( "css-button-disabled", isScrollDownDisabled )
                            ]
                        , onClick ScrollDown
                        , disabled isScrollDownDisabled
                        ]
                        []
                    ]
                ]
            ]
