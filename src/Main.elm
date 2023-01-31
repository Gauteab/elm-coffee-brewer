module Main exposing (main)

import Browser
import Css
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Task
import Time
import Time.Extra as Time



{-
   Mål for sesjonen:
   - Label på input
   - Vise når du skal gå til neste steg
   - Finpusse
-}
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
    div
        [ Attributes.css
            [ Css.width (Css.pct 80)
            , Css.margin2 Css.zero Css.auto
            ]
        ]
        [ h1 [] [ text "Coffee app ☕️" ]
        , case model of
            Init { coffeeInGrams, showErrorMessage } ->
                div []
                    [ label
                        [ Attributes.css
                            [ Css.displayFlex
                            , Css.flexDirection Css.column
                            , Css.alignItems Css.flexStart
                            ]
                        ]
                        [ text "Water (in grams)"
                        , input [ type_ "text", value coffeeInGrams, onInput CoffeeAmountChanged ] []
                        ]
                    , div [ Attributes.css [ Css.color (Css.hex "f00"), Css.marginTop (Css.px 4) ] ]
                        [ text <|
                            if showErrorMessage then
                                "Invalid coffee amount"

                            else
                                ""
                        ]
                    , coffeeInGrams
                        |> String.toInt
                        |> Maybe.map viewRecipe
                        |> Maybe.withDefault viewEmptyRecipe
                    , viewTimer 0
                    ]

            Countdown { startTime, currentTime, coffeeInGrams } ->
                let
                    elapsed =
                        Time.diff Time.Second Time.utc startTime currentTime
                in
                div []
                    [ viewRecipe coffeeInGrams
                    , viewTimer elapsed
                    ]
        ]


viewTimer : Int -> Html Msg
viewTimer elapsed =
    div []
        [ button
            [ onClick StartTimerButtonClicked
            , Attributes.css
                [ Css.backgroundColor Css.transparent
                , Css.border Css.zero
                , Css.fontSize (Css.rem 8)
                , Css.position Css.absolute
                , Css.bottom (Css.rem 6)
                ]
            ]
            [ text (elapsed // 60 |> String.fromInt |> String.padLeft 2 '0')
            , text ":"
            , text (elapsed |> modBy 60 |> String.fromInt |> String.padLeft 2 '0')
            ]
        ]


viewRecipe : Int -> Html msg
viewRecipe coffeeInGrams =
    let
        beansInGrams =
            toFloat coffeeInGrams / 1000.0 * 60
    in
    viewRecipeHelper
        { beansInGrams = String.fromInt (round beansInGrams) ++ " g"
        , waterInGrams = String.fromInt (round (beansInGrams * 2)) ++ " g"
        , pours =
            [ String.fromInt (round (toFloat coffeeInGrams * (300 / 800))) ++ " g"
            , String.fromInt (round (toFloat coffeeInGrams * (500 / 800))) ++ " g"
            , String.fromInt (round (toFloat coffeeInGrams)) ++ " g"
            ]
        }


viewRecipeHelper : { beansInGrams : String, waterInGrams : String, pours : List String } -> Html msg
viewRecipeHelper record =
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            ]
        ]
        [ ol
            [ Attributes.css
                [ Css.listStyle Css.none
                , Css.textAlign Css.right
                ]
            ]
            ([ li [] [ text (record.beansInGrams ++ " \u{1FAD8}") ]
             , li [] [ text (record.waterInGrams ++ " 🌸") ]
             ]
                ++ (record.pours
                        |> List.map (\pour -> li [] [ text (pour ++ " 🌊") ])
                   )
            )
        ]


viewEmptyRecipe : Html msg
viewEmptyRecipe =
    viewRecipeHelper
        { beansInGrams = "--- "
        , waterInGrams = "--- "
        , pours =
            [ "--- "
            , "--- "
            , "--- "
            ]
        }



--- MAIN ---


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Init { coffeeInGrams = "", showErrorMessage = False }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = \_ -> Time.every 1000.0 Tick
        }
