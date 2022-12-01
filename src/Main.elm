module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



--- MODEL ---


type Model
    = Model



--- UPDATE ---


type Msg
    = NoMessageYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMessageYet ->
            ( model, Cmd.none )



--- VIEW ---


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "We're up and running!" ]
        ]



--- MAIN ---


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
