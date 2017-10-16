module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Toy.SimpleTyping as SimpleTyping exposing (..)
import Toy.SimpleParser as SimpleParser
import Dict exposing (Dict)
import Parser


suite : Test
suite =
    describe "SimpleTyping"
        [ describe "fromExp"
            [ test "01" <| testFromExp "1"
            , test "02" <| testFromExp "''"
            , test "03" <| testFromExp "a"
            , test "04" <| testFromExp "f 1"
            , test "05" <| testFromExp "f 1 ''"
            , test "06" <| testFromExp "f a"
            , test "07" <| testFromExp "\\a -> 1"
            , test "08" <| testFromExp "\\a -> a"
            , test "09" <| testFromExp "\\a -> increment a"
            , test "10" <| testFromExp "\\a -> add a 1"
            , test "11" <| testFromExp "\\a -> \\b -> 1"
            , test "12" <| testFromExp "\\a -> \\b -> a"
            , test "13" <| testFromExp "\\a -> \\a -> a"
            , test "14" <| testFromExp "let a = 1; in a"
            , test "15" <| testFromExp "let a = (\\a -> a); in a"
            ]
        , describe "eval"
            [ test "01" <| testEval "a" Dict.empty
            , test "02" <| testEval "\\a -> b" Dict.empty
            , test "03" <| testEval "\\a -> b" (Dict.singleton "b" "Int")
            , test "04" <| testEval "\\a -> a" Dict.empty
            , test "05" <| testEval "(\\a -> '') 1" Dict.empty
            , test "06" <| testEval "(\\a -> a) 1" Dict.empty
            , test "07" <| testEval "(\\a -> f a)" (Dict.singleton "f" "Int -> String")
            , test "08" <| testEval "if a b c" (Dict.singleton "if" "Bool -> a -> a -> a")
            , test "09" <| testEval "a 1" (Dict.singleton "a" "Int")
            , test "10" <| testEval "a 1" (Dict.singleton "a" "a")
            , test "11" <| testEval "a 1" (Dict.singleton "a" "Int -> String")
            , test "12" <| testEval "a 1" (Dict.singleton "a" "String -> Int")
            , test "13" <| testEval "a 1" (Dict.singleton "a" "a -> a")
            ]
        ]


testFromExp : String -> () -> Expectation
testFromExp s _ =
    case Parser.run SimpleParser.expression s of
        Ok exp ->
            exp
                |> SimpleTyping.fromExp 0 Dict.empty
                |> (\( t, _, env ) -> ( t, env ))
                |> (\( t, dep ) -> Debug.log s (formatType t))
                |> always Expect.pass

        Err e ->
            Expect.fail (SimpleParser.formatError e)


testEval : String -> Dict String String -> () -> Expectation
testEval s envSource _ =
    let
        parseResult =
            Parser.run SimpleParser.expression s
                |> Result.andThen
                    (\exp ->
                        envSource
                            |> Dict.map (\_ s -> Parser.run SimpleParser.typeExp s)
                            |> Dict.foldl
                                (\name result results ->
                                    results
                                        |> Result.andThen
                                            (\rs ->
                                                result
                                                    |> Result.map (\r -> Dict.insert name r rs)
                                            )
                                )
                                (Ok Dict.empty)
                            |> Result.map (\dict -> ( exp, dict ))
                    )
    in
        case parseResult of
            Ok ( exp, env_ ) ->
                let
                    ( t, _, dep ) =
                        exp
                            |> SimpleTyping.fromExp 0 Dict.empty

                    env =
                        env_
                            |> Dict.toList
                            |> List.filterMap
                                (\( name, tExp ) ->
                                    dep
                                        |> Dict.get name
                                        |> Maybe.map
                                            (\id ->
                                                ( id, SimpleTyping.fromTypeExp tExp )
                                            )
                                )
                            |> Dict.fromList
                in
                    t
                        |> evaluate env
                        |> Result.map (Tuple.first >> formatType)
                        |> Debug.log s
                        |> always Expect.pass

            Err e ->
                Expect.fail (SimpleParser.formatError e)
