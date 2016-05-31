port module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onInput, onCheck)
import Html.Attributes exposing (..)
import Date exposing (Date)
import Date.Extra.Core exposing (daysInMonth, firstOfNextMonthDate)
import Time exposing (Time, second)
import String


main : Program (Maybe Model)
main =
    Html.programWithFlags
        { init = init
        , update = (\msg model -> withSetStorage (update msg model))
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { date : Float
    , livelihood : Float
    , safety : Float
    , receivedMoneyForNextMonth : Bool
    }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    case savedModel of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            ( (Model 0 0 0 False), Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | ChangeLivelyhood String
    | ChangeSafety String
    | ChangeReceivedMoneyForNextMonth Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Tick newTime ->
            let
                newDate =
                    newTime

                newModel =
                    { model
                        | date = newDate
                    }
            in
                newModel ! []

        ChangeLivelyhood newAusgabe ->
            let
                newAusgabeFloat =
                    Result.withDefault 0 (String.toFloat newAusgabe)
            in
                { model | livelihood = newAusgabeFloat } ! []

        ChangeSafety newSicherheit ->
            let
                newSicherheitFloat =
                    Result.withDefault 0 (String.toFloat newSicherheit)
            in
                { model | safety = newSicherheitFloat } ! []

        ChangeReceivedMoneyForNextMonth value ->
            { model | receivedMoneyForNextMonth = value } ! []



-- PORTS


port setStorage : Model -> Cmd msg


withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage ( model, cmds ) =
    ( model, Cmd.batch [ setStorage model, cmds ] )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        balance =
            if model.receivedMoneyForNextMonth then
                (calculateBalance model)
                    + (calculateBalance { model | date = beginningOfNextMonth model.date })
            else
                calculateBalance model
    in
        div []
            [ h1 [] [ text "Money" ]
            , div []
                [ label []
                    [ text "Livelihood: "
                    , input
                        [ value <| toString <| model.livelihood
                        , onInput ChangeLivelyhood
                        ]
                        []
                    ]
                ]
            , div []
                [ label []
                    [ text "Safety: "
                    , input
                        [ value <| toString <| model.safety
                        , onInput ChangeSafety
                        ]
                        []
                    ]
                ]
            , div []
                [ label []
                    [ text "Money for next month already recieved: "
                    , input
                        [ type' "checkbox"
                        , checked model.receivedMoneyForNextMonth
                        , onCheck ChangeReceivedMoneyForNextMonth
                        ]
                        []
                    ]
                ]
            , text <| toString <| balance
            ]



-- HELPER


beginningOfNextMonth : Float -> Float
beginningOfNextMonth date =
    date
        |> Date.fromTime
        |> firstOfNextMonthDate
        |> Date.toTime


calculateBalance : Model -> Float
calculateBalance model =
    let
        date =
            Date.fromTime model.date

        year =
            Date.year date

        month =
            Date.month date

        day =
            Date.day date

        daysThisMonth =
            toFloat <| daysInMonth year month

        daysLeftThisMonth =
            daysThisMonth - toFloat day

        balance =
            (daysLeftThisMonth / daysThisMonth) * model.livelihood + model.safety
    in
        balance
