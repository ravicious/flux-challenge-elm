module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (class)
import WebSocket
import Json.Decode as Json exposing ((:=))
import Http
import Task


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


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


type alias Model =
    { currentPlanet : Maybe Planet
    , darkJedis : List DarkJedi
    }


firstDarkJediIdToFetch : Int
firstDarkJediIdToFetch =
    3616


init : ( Model, Cmd Msg )
init =
    ( { currentPlanet = Nothing, darkJedis = [] }, fetchDarkJedi firstDarkJediIdToFetch )



-- Update


type Msg
    = VisitPlanet Planet
    | PlanetParsingError String
    | FetchingDarkJediFailed Http.Error
    | AddNewDarkJedi DarkJedi


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
            ( { model | darkJedis = newDarkJedi :: model.darkJedis }, Cmd.none )



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


darkJediToListItem : DarkJedi -> Html Msg
darkJediToListItem darkJedi =
    li [ class "css-slot" ]
        [ h3 [] [ text darkJedi.name ]
        , h6 [] [ text ("Homeworld: " ++ darkJedi.homeworld.name) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "css-root" ]
        [ h1 [ class "css-planet-monitor" ]
            [ text ("Obi-Wan currently on " ++ (planetToText model.currentPlanet))
            ]
        , section [ class "css-scrollable-list" ]
            [ ul [ class "css-slots" ] (List.map darkJediToListItem model.darkJedis)
            , div [ class "css-scroll-buttons" ]
                [ button [ class "css-button-up" ] []
                , button [ class "css-button-down" ] []
                ]
            ]
        ]
