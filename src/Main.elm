module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (class)
import WebSocket
import Json.Decode as Json exposing ((:=))
import Http
import Task
import Roster exposing (Roster)


main : Program Never
main =
    App.program
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


type alias DarkJedi =
    { id : Int
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
    }


firstDarkJediIdToFetch : Int
firstDarkJediIdToFetch =
    3616


init : ( Model, Cmd Msg )
init =
    ( { currentPlanet = Nothing
      , darkJedis = Roster.initialize darkJedisRosterSize
      }
    , fetchDarkJedi firstDarkJediIdToFetch
    )



-- Update


type Msg
    = VisitPlanet Planet
    | PlanetParsingError String
    | FetchingDarkJediFailed Http.Error
    | AddNewDarkJedi DarkJedi


isApprenticeOfLastJedi : DarkJediRoster -> DarkJedi -> Bool
isApprenticeOfLastJedi roster newJedi =
    let
        lastJediApprentice =
            roster |> Roster.last |> (flip Maybe.andThen) .apprentice
    in
        case lastJediApprentice of
            Just apprentice ->
                apprentice.id == newJedi.id

            Nothing ->
                False


isMasterOfFirstJedi : DarkJediRoster -> DarkJedi -> Bool
isMasterOfFirstJedi roster newJedi =
    let
        firstJediMaster =
            roster |> Roster.first |> (flip Maybe.andThen) .master
    in
        case firstJediMaster of
            Just master ->
                master.id == newJedi.id

            Nothing ->
                False


addJediToRoster : DarkJedi -> DarkJediRoster -> Maybe DarkJediRoster
addJediToRoster newJedi roster =
    if Roster.isFull roster then
        Nothing
    else if Roster.isEmpty roster then
        Roster.append newJedi roster
    else if isApprenticeOfLastJedi roster newJedi then
        Roster.append newJedi roster
    else if isMasterOfFirstJedi roster newJedi then
        Roster.prepend newJedi roster
    else
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VisitPlanet newPlanet ->
            ( { model | currentPlanet = Just newPlanet }, Cmd.none )

        PlanetParsingError _ ->
            ( model, Cmd.none )

        FetchingDarkJediFailed _ ->
            ( model, Cmd.none )

        AddNewDarkJedi newDarkJedi ->
            let
                updatedModel =
                    { model
                        | darkJedis =
                            addJediToRoster newDarkJedi model.darkJedis
                                |> Maybe.withDefault model.darkJedis
                    }

                command =
                    case newDarkJedi.apprentice of
                        Just apprentice ->
                            fetchDarkJedi apprentice.id

                        Nothing ->
                            Cmd.none
            in
                ( updatedModel, command )



-- Decoders


planetDecoder : Json.Decoder Planet
planetDecoder =
    Json.object2 Planet ("id" := Json.int) ("name" := Json.string)


darkJediMetaDataDecoder : Json.Decoder DarkJediMetaData
darkJediMetaDataDecoder =
    Json.object2 DarkJediMetaData ("id" := Json.int) ("url" := Json.string)


darkJediDecoder : Json.Decoder DarkJedi
darkJediDecoder =
    Json.object5 DarkJedi
        ("id" := Json.int)
        ("name" := Json.string)
        ("homeworld" := planetDecoder)
        (Json.maybe ("master" := darkJediMetaDataDecoder))
        (Json.maybe ("apprentice" := darkJediMetaDataDecoder))



-- Effects


fetchDarkJedi : Int -> Cmd Msg
fetchDarkJedi id =
    let
        url =
            "http://localhost:3000/dark-jedis/" ++ (toString id)

        handleFailure =
            FetchingDarkJediFailed << Debug.log "Fetching dark jedi failed"
    in
        Task.perform handleFailure AddNewDarkJedi <| Http.get darkJediDecoder url



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


darkJediToListItem : Maybe DarkJedi -> Html Msg
darkJediToListItem darkJedi =
    case darkJedi of
        Just darkJedi ->
            li [ class "css-slot" ]
                [ h3 [] [ text darkJedi.name ]
                , h6 [] [ text ("Homeworld: " ++ darkJedi.homeworld.name) ]
                ]

        Nothing ->
            li [ class "css-slot" ] []


rosterToListItems : DarkJediRoster -> List (Html Msg)
rosterToListItems =
    Roster.toList >> List.map darkJediToListItem


view : Model -> Html Msg
view model =
    div [ class "css-root" ]
        [ h1 [ class "css-planet-monitor" ]
            [ text ("Obi-Wan currently on " ++ (planetToText model.currentPlanet))
            ]
        , section [ class "css-scrollable-list" ]
            [ ul [ class "css-slots" ] (rosterToListItems model.darkJedis)
            , div [ class "css-scroll-buttons" ]
                [ button [ class "css-button-up" ] []
                , button [ class "css-button-down" ] []
                ]
            ]
        ]
