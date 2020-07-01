module Code.Coders exposing (checkDoubles, decodeString, encodeString)

import Code exposing (code)
import Dict exposing (Dict)
import Random exposing (Seed)
import Random.List
import Set exposing (Set)
import String.Extra


type alias Encoding =
    Dict Char (Set String)


type alias Decoding =
    Dict String Char


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
    String.toLower str
        |> String.toList
        |> List.foldl (encode enc) ( "", seed )
        |> Tuple.first


decodeString : String -> String
decodeString str =
    let
        dec =
            decoding
    in
    String.Extra.break 2 str
        |> List.map (decode dec)
        |> String.fromList


encode : Encoding -> Char -> ( String, Seed ) -> ( String, Seed )
encode enc ch ( str, seed ) =
    let
        addToString mA =
            Maybe.withDefault "02" mA
                |> (\s -> String.join "" [ str, s ])

        clean ( mA, s ) =
            ( addToString mA
            , s
            )
    in
    Dict.get ch enc
        |> Maybe.map Set.toList
        |> Maybe.withDefault (List.singleton "55")
        |> Random.List.choose
        |> Random.map Tuple.first
        |> (\g -> Random.step g seed)
        |> clean


decode : Decoding -> String -> Char
decode dec num =
    Dict.get num dec
        |> Maybe.withDefault '*'


encoding : Encoding
encoding =
    let
        enc : Encoding
        enc =
            Dict.empty

        add : ( Char, String ) -> Encoding -> Dict Char (Set String)
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


doubles : List String -> List String
doubles ints =
    let
        processInt : String -> ( Set String, Set String ) -> ( Set String, Set String )
        processInt i ( areIn, doubleInts ) =
            if Set.member i areIn then
                ( areIn, Set.insert i doubleInts )

            else
                ( Set.insert i areIn, doubleInts )
    in
    List.foldl processInt ( Set.empty, Set.empty ) ints
        |> Tuple.second
        |> Set.toList


checkDoubles : List String
checkDoubles =
    List.map Tuple.second code
        |> doubles
