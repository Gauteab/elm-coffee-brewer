module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Time
import Time.Extra as Time



--- MODEL ---


type Model
    = Init { showErrorMessage : Bool, coffeeInGrams : String }
    | Countdown TimerModel


type alias TimerModel =
    { startTime : Time.Posix
    , currentTime : Time.Posix
    , coffeeInGrams : Int
    }



--- UPDATE ---


type Msg
    = Tick Time.Posix
    | StartTimerButtonClicked
    | StartingTimeReceived Time.Posix
    | CoffeeAmountChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CoffeeAmountChanged newCoffeeAmount ->
            case model of
                Init { coffeeInGrams } ->
                    ( Init
                        { coffeeInGrams = newCoffeeAmount
                        , showErrorMessage = False
                        }
                    , Cmd.none
                    )

                Countdown timerModel ->
                    ( model, Cmd.none )

        Tick newTime ->
            case model of
                Init _ ->
                    ( model, Cmd.none )

                Countdown timerModel ->
                    ( Countdown { timerModel | currentTime = newTime }, Cmd.none )

        StartTimerButtonClicked ->
            ( model, Time.now |> Task.perform StartingTimeReceived )

        StartingTimeReceived now ->
            case model of
                Init { coffeeInGrams } ->
                    case coffeeInGrams |> String.toInt of
                        Nothing ->
                            ( Init { coffeeInGrams = coffeeInGrams, showErrorMessage = True }, Cmd.none )

                        Just int ->
                            ( Countdown
                                { startTime = now
                                , currentTime = now
                                , coffeeInGrams = int
                                }
                            , Cmd.none
                            )

                Countdown timerModel ->
                    ( Countdown
                        { startTime = now
                        , currentTime = now
                        , coffeeInGrams =
                            timerModel.coffeeInGrams
                        }
                    , Cmd.none
                    )



--- VIEW ---


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Coffee app ??????" ]
        , button [ onClick StartTimerButtonClicked ] [ text "Start" ]
        , case model of
            Init { coffeeInGrams, showErrorMessage } ->
                div []
                    [ input [ type_ "text", value coffeeInGrams, onInput CoffeeAmountChanged ] []
                    , div []
                        [ text <|
                            if showErrorMessage then
                                "Invalid coffee amount"

                            else
                                ""
                        ]
                    , coffeeInGrams |> String.toInt |> Maybe.map viewRecipe |> Maybe.withDefault (text "")
                    ]

            Countdown { startTime, currentTime, coffeeInGrams } ->
                let
                    elapsed =
                        Time.diff Time.Second Time.utc startTime currentTime
                in
                div []
                    [ span []
                        [ text (elapsed // 60 |> String.fromInt)
                        , text ":"
                        , text (elapsed |> modBy 60 |> String.fromInt |> String.padLeft 2 '0')
                        ]
                    , viewRecipe coffeeInGrams
                    ]
        ]


viewRecipe : Int -> Html msg
viewRecipe coffeeInGrams =
    let
        beansInGrams =
            toFloat coffeeInGrams / 1000.0 * 60
    in
    ol [ style "list-style" "none" ]
        [ li [] [ text (String.fromFloat beansInGrams ++ "g \u{1FAD8}") ]
        , li [] [ text (String.fromFloat (beansInGrams * 2) ++ "g ????????") ]
        , li [] [ text (String.fromFloat (toFloat coffeeInGrams * (300 / 800)) ++ "g ????") ]
        , li [] [ text (String.fromFloat (toFloat coffeeInGrams * (500 / 800)) ++ "g ????") ]
        , li [] [ text (String.fromFloat (toFloat coffeeInGrams) ++ "g ????") ]
        ]



--- MAIN ---


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Init { coffeeInGrams = "", showErrorMessage = False }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Time.every 1000.0 Tick
        }
