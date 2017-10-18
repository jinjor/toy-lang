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
            , test "05.0" <| testFromExp "(f a) 1"
            , test "05.1" <| testFromExp "f (a 1) 2"
            , test "05.2" <| testFromExp "(f a) 1 2"
            , test "05.3" <| testFromExp "f 1 (a 2)"
            , test "06" <| testFromExp "f a"
            , test "07" <| testFromExp "\\a -> 1"
            , test "08" <| testFromExp "\\a -> a"
            , test "09" <| testFromExp "\\a -> increment a"
            , test "10" <| testFromExp "\\a -> add a 1"
            , test "11" <| testFromExp "\\a -> \\b -> 1"
            , test "12" <| testFromExp "\\a -> \\b -> a"
            , test "13" <| testFromExp "\\a -> \\a -> a"
            , test "14" <| testFromExp "do a = 1; return a"
            , test "15" <| testFromExp "do a = (\\a -> a); return a"
            , test "16" <| testFromExp "do a = (\\a -> a); return f (a 1) (a '')"
            , test "17" <| testFromExp "(\\a -> f (a 1) (a '')) (\\a -> a)"
            , test "18" <| testFromExp "do a=1;b=2; return add a b"
            ]
        , describe "eval"
            [ test "01" <| testEval "a" [] ""
            , test "02" <| testEval "\\a -> b" [] ""
            , test "03" <| testEval "\\a -> b" [ "b" => "Int" ] ""
            , test "04" <| testEval "\\a -> a" [] ""
            , test "05" <| testEval "(\\a -> '') 1" [] "String"
            , test "06" <| testEval "(\\a -> a) 1" [] "Int"
            , test "07" <| testEval "(\\a -> f a)" [ "f" => "Int -> String" ] "(Int -> String)"
            , test "08" <| testEval "if a b c" [ "if" => "Bool -> a -> a -> a" ] ""
            , test "08.1" <| testEval "if 0 '' ''" [ "if" => "Int -> a -> a -> a" ] "String"
            , test "08.2" <| testEval "if 0 0 ''" [ "if" => "Int -> a -> a -> a" ] ""
            , test "08.3" <| testEval "if 0 a ''" [ "if" => "Int -> a -> a -> a", "a" => "String" ] ""
            , test "08.4" <| testEval "if 0 a ''" [ "if" => "Int -> a -> a -> a", "a" => "Int" ] ""
            , test "08.5" <| testEval "if 0 '' a" [ "if" => "Int -> a -> a -> a", "a" => "Int" ] ""
            , test "08.6" <| testEval "if 0 '' a" [ "if" => "Int -> a -> a -> a" ] "String"
            , test "08.7" <| testEval "if 0 a ''" [ "if" => "Int -> a -> a -> a" ] "String"
            , test "09" <| testEval "a 1" [ "a" => "Int" ] ""
            , test "10" <| testEval "a 1" [ "a" => "a" ] ""
            , test "11" <| testEval "a 1" [ "a" => "Int -> String" ] "String"
            , test "12" <| testEval "a 1" [ "a" => "String -> Int" ] ""
            , test "13" <| testEval "a 1" [ "a" => "a -> a" ] ""
            , test "14" <| testEval "do a = (\\a -> a); return f (a 1) (a '')" [] ""
            , test "15" <| testEval "(\\a -> a 1) (\\a -> a)" [] ""
            , test "16" <| testEval "(\\a -> f (a 1)) (\\a -> a)" [] ""
            , test "17" <| testEval "(\\a -> f (a 1) (a '')) (\\a -> a)" [] ""
            , test "18" <| testEval "(\\a -> f (a 1) (a ''))" [] ""
            , test "19" <| testEval "do a=1;b=2;return add a b" [] ""
            , test "20" <| testEval "do a=1;a='';return a" [] "String"
            , test "21" <| testEval "do a=1;b=a;b=b;return b" [] "Int"
            , test "22" <|
                testEval "map toString"
                    [ "map" => "(a -> b) -> a -> b"
                    , "toString" => "Int -> String"
                    ]
                    "(Int -> String)"
            , test "23" <|
                testEval "map 1"
                    [ "map" => "(a -> b) -> a -> b"
                    ]
                    ""
            , test "24" <|
                testEval "map a"
                    [ "map" => "(a -> b) -> a -> b"
                    , "a" => "Int -> String -> Bool"
                    ]
                    "(Int -> (String -> Bool))"
            ]
        ]


(=>) =
    (,)


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


testEval : String -> List ( String, String ) -> String -> () -> Expectation
testEval s envSource_ expected _ =
    let
        envSource =
            Dict.fromList envSource_

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
                    case evaluate env t of
                        Ok ( t, env ) ->
                            if expected == "" then
                                Debug.log s ( formatType t, Dict.map (\_ -> formatType) env )
                                    |> always Expect.pass
                            else
                                Expect.equal expected (formatType t)

                        Err e ->
                            Debug.log s e
                                |> always Expect.pass

            Err e ->
                Expect.fail (SimpleParser.formatError e)
