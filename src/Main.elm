module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (class)


main : Program Never
main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- Model


type alias Model =
    ()


model : Model
model =
    ()



-- Update


type Msg
    = None


update : Msg -> Model -> Model
update msg model =
    model



-- View


view : Model -> Html Msg
view model =
    div [ class "css-root" ]
        [ h1 [ class "css-planet-monitor" ] [ text "Obi-Wan current on Tatooine" ]
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
