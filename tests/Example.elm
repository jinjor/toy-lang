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
            [ testFromExp "1"
            , testFromExp "''"
            , testFromExp "a"
            , testFromExp "f 1"
            , testFromExp "f 1 ''"
            , testFromExp "(f a) 1"
            , testFromExp "f (a 1) 2"
            , testFromExp "(f a) 1 2"
            , testFromExp "f 1 (a 2)"
            , testFromExp "f a"
            , testFromExp "\\a -> 1"
            , testFromExp "\\a -> a"
            , testFromExp "\\a -> increment a"
            , testFromExp "\\a -> add a 1"
            , testFromExp "\\a -> \\b -> 1"
            , testFromExp "\\a -> \\b -> a"
            , testFromExp "\\a -> \\a -> a"
            , testFromExp "do a = 1; return a"
            , testFromExp "do a = (\\a -> a); return a"
            , testFromExp "do a = (\\a -> a); return f (a 1) (a '')"
            , testFromExp "(\\a -> f (a 1) (a '')) (\\a -> a)"
            , testFromExp "do a=1;b=2; return add a b"
            ]
        , describe "eval"
            [ testEval "a" [] ""
            , testEval "\\a -> b" [] ""
            , testEval "\\a -> b" [ "b" => "Int" ] ""
            , testEval "\\a -> a" [] ""
            , testEval "(\\a -> '') 1" [] "String"
            , testEval "(\\a -> a) 1" [] "Int"
            , testEval "(\\a -> f a)" [ "f" => "Int -> String" ] "(Int -> String)"
            , testEval "if a b c" [ "if" => "Bool -> a -> a -> a" ] ""
            , testEval "if 0 '' ''" [ "if" => "Int -> a -> a -> a" ] "String"
            , testEval "if 0 0 ''" [ "if" => "Int -> a -> a -> a" ] "expected Int"
            , testEval "if 0 a ''" [ "if" => "Int -> a -> a -> a", "a" => "String" ] "String"
            , testEval "if 0 a ''" [ "if" => "Int -> a -> a -> a", "a" => "Int" ] "expected Int"
            , testEval "if 0 '' a" [ "if" => "Int -> a -> a -> a", "a" => "Int" ] "expected String"
            , testEval "if 0 '' a" [ "if" => "Int -> a -> a -> a" ] "String"
            , testEval "if 0 a ''" [ "if" => "Int -> a -> a -> a" ] "String"
            , testEval "a 1" [ "a" => "Int" ] ""
            , testEval "a 1" [ "a" => "a" ] ""
            , testEval "a 1" [ "a" => "Int -> String" ] "String"
            , testEval "a 1" [ "a" => "String -> Int" ] "expected String"
            , testEval "a 1" [ "a" => "a -> a" ] "Int"
            , testEval "do a = (\\a -> a); return f (a 1) (a '')" [] ""
            , testEval "(\\a -> a 1) (\\a -> a)" [] ""
            , testEval "(\\a -> f (a 1)) (\\a -> a)" [] ""
            , testEval "(\\a -> f (a 1) (a '')) (\\a -> a)" [] ""
            , testEval "(\\a -> f (a 1) (a ''))" [] ""
            , testEval "do a=1;b=2;return add a b" [] ""
            , testEval "do a=1;a='';return a" [] "String"
            , testEval "do a=1;b=a;b=b;return b" [] "Int"
            , testEval "map toString"
                [ "map" => "(a -> b) -> a -> b"
                , "toString" => "Int -> String"
                ]
                "(Int -> String)"
            , testEval "map 1"
                [ "map" => "(a -> b) -> a -> b"
                ]
                "few"
            , testEval "map a"
                [ "map" => "(a -> b) -> a -> b"
                , "a" => "Int -> String -> Bool"
                ]
                "(Int -> (String -> Bool))"
            ]
        ]


(=>) =
    (,)


logParseResult : String -> ( Type, Int, Dict String Int ) -> ( Type, Int, Dict String Int )
logParseResult s (( t, _, dep ) as r) =
    let
        _ =
            Debug.log ("[parsed]    " ++ s)
                (formatType t
                    ++ (if Dict.isEmpty dep then
                            ""
                        else
                            " with " ++ formatDict identity toString dep
                       )
                )
    in
        r


testFromExp : String -> Test
testFromExp s =
    test s
        (\_ ->
            case Parser.run SimpleParser.expression s of
                Ok exp ->
                    exp
                        |> SimpleTyping.fromExp 0 Dict.empty
                        |> logParseResult s
                        |> always Expect.pass

                Err e ->
                    Expect.fail (SimpleParser.formatError e)
        )


testEval : String -> List ( String, String ) -> String -> Test
testEval s envSource expected =
    test (s ++ " with " ++ toString envSource)
        (\_ ->
            let
                parseResult =
                    Parser.run SimpleParser.expression s
                        |> Result.andThen
                            (\exp ->
                                parseEnv envSource
                                    |> Result.map (\dict -> ( exp, dict ))
                            )
            in
                case parseResult of
                    Ok ( exp, env_ ) ->
                        let
                            ( t, _, dep ) =
                                exp
                                    |> SimpleTyping.fromExp 0 Dict.empty
                                    |> logParseResult s

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
                                    let
                                        _ =
                                            Debug.log "" ""

                                        _ =
                                            Debug.log ("[evauated]  " ++ s)
                                                (formatType t
                                                    ++ (if Dict.isEmpty env then
                                                            ""
                                                        else
                                                            " with " ++ formatDict toString formatType env
                                                       )
                                                )
                                    in
                                        if expected == "" then
                                            Expect.pass
                                        else
                                            Expect.equal expected (formatType t)

                                Err e ->
                                    let
                                        _ =
                                            Debug.log "" ""

                                        _ =
                                            Debug.log s e
                                    in
                                        if expected == "" then
                                            Expect.pass
                                        else
                                            e
                                                |> String.contains expected
                                                |> Expect.true (e ++ " should contain " ++ expected)

                    Err e ->
                        Expect.fail (SimpleParser.formatError e)
        )


parseEnv : List ( String, String ) -> Result Parser.Error (Dict String SimpleParser.TypeExp)
parseEnv envSource =
    Dict.fromList envSource
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


formatDict : (comparable -> String) -> (b -> String) -> Dict comparable b -> String
formatDict formatKey formatValue dict =
    "{ "
        ++ (dict
                |> Dict.toList
                |> List.map
                    (\( key, value ) ->
                        formatKey key ++ " => " ++ formatValue value
                    )
                |> String.join ", "
           )
        ++ " }"
