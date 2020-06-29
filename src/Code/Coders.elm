module Code.Coders exposing (checkDoubles, decodeString, encodeString)

import Code exposing (code)
import Dict exposing (Dict)
import Random exposing (Seed)
import Random.List
import Set exposing (Set)


type alias Encoding =
    Dict Char (Set Int)


type alias Decoding =
    Dict Int Char


initialSeed : Seed
initialSeed =
    Random.initialSeed 45333


encodeString : String -> String
encodeString str =
    let
        seed =
            initialSeed

        enc =
            encoding
    in
    String.toList str
        |> List.foldl (encode enc) ( "", seed )
        |> Tuple.first


decodeString : String -> String
decodeString str =
    let
        dec =
            decoding
    in
    String.split " " str
        |> List.map (decode dec)
        |> String.fromList


encode : Encoding -> Char -> ( String, Seed ) -> ( String, Seed )
encode enc ch ( str, seed ) =
    let
        addToString mA =
            Maybe.withDefault "002" mA
                |> (\s -> String.join " " [ str, s ])

        clean ( mA, s ) =
            ( addToString mA
            , s
            )
    in
    Dict.get ch enc
        |> Maybe.map Set.toList
        |> Maybe.withDefault (List.singleton 1)
        |> List.map String.fromInt
        |> Random.List.choose
        |> Random.map Tuple.first
        |> (\g -> Random.step g seed)
        |> clean


decode : Decoding -> String -> Char
decode dec num =
    String.toInt num
        |> Maybe.withDefault 1
        |> (\k -> Dict.get k dec)
        |> Maybe.withDefault '*'


encoding : Encoding
encoding =
    let
        enc : Dict Char (Set Int)
        enc =
            Dict.empty

        add : ( Char, Int ) -> Encoding -> Dict Char (Set Int)
        add ( k, v ) d =
            Dict.get k d
                |> Maybe.withDefault Set.empty
                |> Set.insert v
                |> (\s -> Dict.insert k s d)
    in
    List.foldl add enc code


decoding : Decoding
decoding =
    let
        flip ( a, b ) =
            ( b, a )
    in
    List.map flip code
        |> Dict.fromList


doubles : List Int -> List Int
doubles ints =
    let
        processInt : Int -> ( Set Int, Set Int ) -> ( Set Int, Set Int )
        processInt i ( areIn, doubleInts ) =
            if Set.member i areIn then
                ( areIn, Set.insert i doubleInts )

            else
                ( Set.insert i areIn, doubleInts )
    in
    List.foldl processInt ( Set.empty, Set.empty ) ints
        |> Tuple.second
        |> Set.toList


checkDoubles : List Int
checkDoubles =
    List.map Tuple.second code
        |> doubles
