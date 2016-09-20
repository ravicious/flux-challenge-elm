module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (class)
import WebSocket
import Json.Decode as Json exposing ((:=))


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


type alias Model =
    Maybe Planet


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )



-- Update


type Msg
    = VisitPlanet Planet
    | PlanetParsingError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg currentPlanet =
    case msg of
        VisitPlanet newPlanet ->
            ( Just newPlanet, Cmd.none )

        PlanetParsingError _ ->
            ( currentPlanet, Cmd.none )



-- Subscriptions


planetDecoder : Json.Decoder Planet
planetDecoder =
    Json.object2 Planet ("id" := Json.int) ("name" := Json.string)


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


view : Model -> Html Msg
view model =
    div [ class "css-root" ]
        [ h1 [ class "css-planet-monitor" ]
            [ text ("Obi-Wan currently on " ++ (planetToText model))
            ]
        , section [ class "css-scrollable-list" ]
            [ ul [ class "css-slots" ]
                [ li [ class "css-slot" ]
                    [ h3 [] [ text "Jorak Uln" ]
                    , h6 [] [ text "Homeworld: Korriban" ]
                    ]
                , li [ class "css-slot" ]
                    [ h3 [] [ text "Skere Kaan" ]
                    , h6 [] [ text "Homeworld: Coruscant" ]
                    ]
                , li [ class "css-slot" ]
                    [ h3 [] [ text "Na'daz" ]
                    , h6 [] [ text "Homeworld: Ryloth" ]
                    ]
                , li [ class "css-slot" ]
                    [ h3 [] [ text "Kas'im" ]
                    , h6 [] [ text "Homeworld: Nal Hutta" ]
                    ]
                , li [ class "css-slot" ]
                    [ h3 [] [ text "Darth Bane" ]
                    , h6 [] [ text "Homeworld: Apatros" ]
                    ]
                ]
            , div [ class "css-scroll-buttons" ]
                [ button [ class "css-button-up" ] []
                , button [ class "css-button-down" ] []
                ]
            ]
        ]
