module Main exposing (main)

-- https://ellie-app.com/9dQLRDdm9FDa1

import Browser
import Code.Coders exposing (checkDoubles, decodeString, encodeString)
import Html exposing (Html, div, h1, h2, input, label, span, text, textarea)
import Html.Attributes exposing (checked, cols, name, placeholder, rows, style, type_)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { decoded : String
    , encoded : String
    , direction : Direction
    }


type Direction
    = Encoding
    | Decoding


initialText : String
initialText =
    "blablablabla bla"


initialModel : Model
initialModel =
    { decoded = initialText
    , encoded = encodeString initialText
    , direction = Encoding
    }


type Msg
    = Toggle Direction
    | NewInput String


directionToString : Direction -> String
directionToString direction =
    case direction of
        Encoding ->
            "coderen"

        Decoding ->
            "decoderen"


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle Encoding ->
            { model | direction = Encoding }

        Toggle Decoding ->
            { model | direction = Decoding }

        NewInput str ->
            case model.direction of
                Encoding ->
                    { decoded = str
                    , encoded = encodeString str
                    , direction = model.direction
                    }

                Decoding ->
                    { decoded = decodeString str
                    , encoded = str
                    , direction = model.direction
                    }


toggle : Model -> Direction -> Html Msg
toggle model direction =
    let
        buttonText =
            directionToString direction

        isSelected =
            case ( model.direction, direction ) of
                ( Encoding, Encoding ) ->
                    True

                ( Decoding, Decoding ) ->
                    True

                _ ->
                    False
    in
    label []
        [ input [ type_ "radio", name "toggleDirection", onClick (Toggle direction), checked isSelected ] []
        , text buttonText
        ]


view : Model -> Html Msg
view model =
    let
        ( title, placeholderText, result ) =
            case model.direction of
                Encoding ->
                    ( "te coderen tekst"
                    , model.decoded
                    , model.encoded
                    )

                Decoding ->
                    ( "te decoderen code"
                    , model.encoded
                    , model.decoded
                    )
    in
    div [ style "margin" "20px" ]
        [ div [] [ h1 [] [ text "Ebe's Codeermachine" ] ]
        , errors
        , div []
            [ toggle model Encoding
            , toggle model Decoding
            ]
        , div []
            [ h2 [] [ text title ]
            , textarea
                [ placeholder placeholderText
                , onInput NewInput
                , rows 10
                , cols 30
                ]
                []
            ]
        , div []
            [ h2 [] [ text "resultaat" ]
            , text result
            ]
        ]


errors : Html Msg
errors =
    let
        doubles =
            checkDoubles

        doublesString d =
            String.join ", " d
                |> (++) "er zijn getallen meerdere malen gebruikt: "

        error d =
            span [] [ text (doublesString d) ]
    in
    if List.length doubles > 0 then
        div
            [ style "background-color" "red"
            , style "color" "white"
            , style "height" "90px"
            , style "width" "100%"
            ]
            [ error doubles ]

    else
        div [] []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
