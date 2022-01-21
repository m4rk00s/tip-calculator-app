module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img, input, text)
import Html.Attributes exposing (class, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Round


type Tip
    = Tip5
    | Tip10
    | Tip15
    | Tip25
    | Tip50
    | Custom Float


type alias Model =
    { bill : Float
    , tip : Tip
    , numOfPeople : Int
    }


type Msg
    = Reset
    | ChangeBill Float
    | ChangeTip Tip
    | ChangeNumOfPeople Int


type IconName
    = Dollar
    | Person


imageByIconName : Maybe IconName -> Html Msg
imageByIconName iconName =
    case iconName of
        Just Dollar ->
            img [ src "/icon-dollar.svg" ] []

        Just Person ->
            img [ src "/icon-person.svg" ] []

        Nothing ->
            img [] []


tipFloat : Tip -> Float
tipFloat input =
    case input of
        Tip5 ->
            0.05

        Tip10 ->
            0.1

        Tip15 ->
            0.15

        Tip25 ->
            0.25

        Tip50 ->
            0.5

        Custom val ->
            val / 100


calculateTipAmountPerPerson : Model -> Float
calculateTipAmountPerPerson input =
    if .numOfPeople input > 0 then
        .bill input * tipFloat (.tip input) / toFloat (.numOfPeople input)

    else
        0


calculateTotalPerPerson : Model -> Float
calculateTotalPerPerson input =
    if .numOfPeople input > 0 then
        (.bill input + (.bill input * tipFloat (.tip input))) / toFloat (.numOfPeople input)

    else
        0


viewButton : String -> Bool -> Msg -> Html Msg
viewButton contentText isActive msg =
    button
        [ class "w-full h-12 rounded-md text-2xl font-bold"
        , class
            (if isActive then
                "bg-[#26C2AE] text-[#00474B]"

             else
                "bg-[#00474B] text-white"
            )
        , onClick msg
        ]
        [ text contentText ]


viewInputNumber : Maybe IconName -> String -> String -> (String -> Msg) -> Html Msg
viewInputNumber iconName placeHolderValue inputValue msg =
    div [ class "relative" ]
        [ div [ class "absolute top-1/2 transform origin-center -translate-y-1/2 ml-5" ]
            [ imageByIconName iconName ]
        , input
            [ type_ "number"
            , class "w-full bg-[#F3F9FA] placeholder:text-[#547878] rounded-md text-right font-bold px-4 h-12 text-2xl"
            , value inputValue
            , onInput msg
            , placeholder placeHolderValue
            ]
            []
        ]


initialModel : Model
initialModel =
    Model 0 Tip5 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            initialModel

        ChangeBill x ->
            { model | bill = x }

        ChangeTip x ->
            { model | tip = x }

        ChangeNumOfPeople x ->
            { model | numOfPeople = x }


view : Model -> Html Msg
view model =
    div [ class "bg-[#C5E4E7] h-full overflow-auto flex flex-col" ]
        [ div [ class "py-12" ] [ img [ src "/logo.svg", class "mx-auto" ] [] ]
        , div [ class "lg:self-center lg:max-w-[57.5rem]" ]
            [ div [ class "bg-white flex flex-col flex-1 rounded-3xl p-8 h-auto lg:gap-12 lg:flex-row lg:flex" ]
                [ div [ class "lg:flex-1" ]
                    [ div []
                        [ div [ class "font-bold text-[#5E7A7D]" ] [ text "Bill" ]
                        , div [ class "mt-1" ]
                            [ viewInputNumber
                                (Just Dollar)
                                ""
                                (String.fromFloat (.bill model))
                                (\x -> ChangeBill (withDefault 0 (String.toFloat x)))
                            ]
                        ]
                    , div [ class "mt-8" ]
                        [ div [ class "font-bold text-[#5E7A7D]" ] [ text "Select Tip %" ]
                        , div [ class "grid grid-cols-2 gap-4 mt-4" ]
                            [ viewButton "5%" (.tip model == Tip5) (ChangeTip Tip5)
                            , viewButton "10%" (.tip model == Tip10) (ChangeTip Tip10)
                            , viewButton "15%" (.tip model == Tip15) (ChangeTip Tip15)
                            , viewButton "25%" (.tip model == Tip25) (ChangeTip Tip25)
                            , viewButton "50%" (.tip model == Tip50) (ChangeTip Tip50)
                            , viewInputNumber
                                Nothing
                                "Custom"
                                (case .tip model of
                                    Custom x ->
                                        String.fromFloat x

                                    _ ->
                                        ""
                                )
                                (\x -> ChangeTip (Custom (withDefault 0 (String.toFloat x))))
                            ]
                        ]
                    , div [ class "mt-8" ]
                        [ div [ class "font-bold text-[#5E7A7D]" ] [ text "Number of People" ]
                        , div [ class "mt-1" ]
                            [ viewInputNumber
                                (Just Person)
                                ""
                                (String.fromInt (.numOfPeople model))
                                (\x -> ChangeNumOfPeople (withDefault 0 (String.toInt x)))
                            ]
                        ]
                    ]
                , div [ class "mt-8 lg:flex-1 lg:mt-0" ]
                    [ div [ class "flex flex-col gap-5 bg-[#00474B] text-white -mx-2 lg:mx-0 rounded-2xl p-6 lg:h-full lg:p-10" ]
                        [ div [ class "flex justify-between align-middle lg:py-4" ]
                            [ div []
                                [ div [] [ text "Tip Amount" ]
                                , div
                                    [ class "text-sm font-bold text-[#7F9D9F]" ]
                                    [ text "/ person" ]
                                ]
                            , div
                                [ class "text-right text-[#26C2AE] font-bold text-3xl lg:text-5xl" ]
                                [ text ("$" ++ Round.round 2 (calculateTipAmountPerPerson model)) ]
                            ]
                        , div [ class "flex justify-between align-middle lg:py-4" ]
                            [ div []
                                [ div [] [ text "Total" ]
                                , div
                                    [ class "text-sm font-bold text-[#7F9D9F]" ]
                                    [ text "/ person" ]
                                ]
                            , div
                                [ class "text-right text-[#26C2AE] font-bold text-3xl lg:text-5xl" ]
                                [ text ("$" ++ Round.round 2 (calculateTotalPerPerson model)) ]
                            ]
                        , div [ class "col-span-2 lg:mt-auto" ] [ viewButton "RESET" True Reset ]
                        ]
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }
