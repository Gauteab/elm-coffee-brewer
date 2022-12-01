module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task
import Time
import Time.Extra as Time



--- MODEL ---


type Model
    = Init
    | Countdown TimerModel


type alias TimerModel =
    { startTime : Time.Posix
    , currentTime : Time.Posix
    }



--- UPDATE ---


type Msg
    = Tick Time.Posix
    | StartTimerButtonClicked
    | TimeNowReceived Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            case model of
                Init ->
                    ( model, Cmd.none )

                Countdown timerModel ->
                    ( Countdown { timerModel | currentTime = newTime }, Cmd.none )

        StartTimerButtonClicked ->
            ( model, Time.now |> Task.perform TimeNowReceived )

        TimeNowReceived now ->
            ( Countdown { startTime = now, currentTime = now }, Cmd.none )



--- VIEW ---


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Coffee app ☕️" ]
        , button [ onClick StartTimerButtonClicked ] [ text "Start" ]
        , case model of
            Init ->
                text ""

            Countdown { startTime, currentTime } ->
                let
                    elapsed =
                        Time.diff Time.Second Time.utc startTime currentTime
                in
                span []
                    [ text (elapsed // 60 |> String.fromInt)
                    , text ":"
                    , text (elapsed |> modBy 60 |> String.fromInt |> String.padLeft 2 '0')
                    ]
        ]



--- MAIN ---


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Init, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Time.every 1000.0 Tick
        }
