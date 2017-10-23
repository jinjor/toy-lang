module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Toy.SimpleTyping as SimpleTyping exposing (..)
import Toy.Parser as ToyParser
import Toy.Formatter as Formatter
import Dict exposing (Dict)
import Parser


suite : Test
suite =
    describe "SimpleTyping"
        [ describe "fromExp"
            [ testFromExp "1"
            , testFromExp "\"\""
            , testFromExp "a"
            , testFromExp "f 1"
            , testFromExp "f 1 \"\""
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
            , testFromExp "do a = (\\a -> a); return f (a 1) (a \"\")"
            , testFromExp "(\\a -> f (a 1) (a \"\")) (\\a -> a)"
            , testFromExp "do a=1;b=2; return add a b"
            ]
        , describe "eval"
            [ testEval "a" [] ""
            , testEval "\\a -> b" [] ""
            , testEval "\\a -> b" [ "b" => "Int" ] ""
            , testEval "\\a -> a" [] ""
            , testEval "(\\a -> \"\") 1" [] "String"
            , testEval "(\\a -> a) 1" [] "Int"
            , testEval "(\\a -> f a)" [ "f" => "Int -> String" ] "(Int -> String)"
            , testEval "(\\a -> a) (f 1)" [ "f" => "Int -> Int" ] "Int"
            , testEval "if a b c" [ "if" => "Bool -> a -> a -> a" ] ""
            , testEval "if 0 \"\" \"\"" [ "if" => "Int -> a -> a -> a" ] "String"
            , testEval "if 1 0 \"\"" [ "if" => "Int -> a -> a -> a" ] "Mismatch"
            , testEval "if 2 a \"\"" [ "if" => "Int -> a -> a -> a", "a" => "String" ] "String"
            , testEval "if 3 a \"\"" [ "if" => "Int -> a -> a -> a", "a" => "Int" ] "Mismatch"
            , testEval "if 4 \"\" a" [ "if" => "Int -> a -> a -> a", "a" => "Int" ] "Mismatch"
            , testEval "if 5 \"\" a" [ "if" => "Int -> a -> a -> a" ] "String"
            , testEval "if 6 a \"\"" [ "if" => "Int -> a -> a -> a" ] "String"
            , testEval "a 1" [ "a" => "Int" ] "Argument"
            , testEval "a 1" [ "a" => "a" ] ""
            , testEval "a 1" [ "a" => "Int -> String" ] "String"
            , testEval "a 1" [ "a" => "String -> Int" ] "Mismatch"
            , testEval "a 1" [ "a" => "a -> a" ] "Int"
            , testEval "do a = (\\a -> a); return f (a 1) (a \"\")" [] ""
            , testEval "(\\a -> a 1) (\\a -> a)" [] "Int"
            , testEval "(\\a -> f (a 1)) (\\a -> a)" [] ""
            , testEval "(\\a -> f (a 1) (a \"\")) (\\a -> a)" [] ""
            , testEval "(\\a -> f (a 1) (a \"\"))" [] ""
            , testEval "f (a 1) (a \"\")" [ "a" => "a -> a", "f" => "b -> c -> b -> c" ] "(Int -> String)"
            , testEval "do a=1;b=2;return add a b" [] ""
            , testEval "do a=1;a=\"\";return a" [] "String"
            , testEval "do a=1;b=a;b=b;return b" [] "Int"
            , testEval "map toString"
                [ "map" => "(a -> b) -> a -> b"
                , "toString" => "Int -> String"
                ]
                "(Int -> String)"
            , testEval "map 1"
                [ "map" => "(a -> b) -> a -> b"
                ]
                "Few"
            , testEval "map a"
                [ "map" => "(a -> b) -> a -> b"
                , "a" => "Int -> String -> Bool"
                ]
                "(Int -> (String -> Bool))"
            , testEval "do f = \\a -> a;return g (f 1) (f \"\")"
                [ "g" => "Int -> String -> Bool" ]
                "Bool"
            , testEval "a 1" [ "a" => "a -> A a" ] "A Int"
            , testEval "map toString list"
                [ "map" => "(a -> b) -> List a -> List b"
                , "toString" => "Int -> String"
                , "list" => "List Int"
                ]
                "List String"
            , testEval "map toString (cons 1 nil)"
                [ "map" => "(a -> b) -> List a -> List b"
                , "toString" => "Int -> String"
                , "cons" => "a -> List a -> List a"
                , "nil" => "List a"
                ]
                "List String"
            ]
        ]


(=>) =
    (,)


logParseResult : String -> ( Type, Int, Dict String Int ) -> ( Type, Int, Dict String Int )
logParseResult s (( t, _, dep ) as r) =
    let
        _ =
            Debug.log ("[parsed]  " ++ s)
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
            case Parser.run ToyParser.expression s of
                Ok exp ->
                    exp
                        |> SimpleTyping.fromOriginalExp 0 Dict.empty
                        |> logParseResult s
                        |> always Expect.pass

                Err e ->
                    Expect.fail (ToyParser.formatError e)
        )


testEval : String -> List ( String, String ) -> String -> Test
testEval s envSource_ expected =
    let
        envSource =
            Dict.fromList envSource_

        input =
            (s ++ " with " ++ formatDict identity identity envSource)

        parseResult =
            Parser.run ToyParser.expression s
                |> Result.andThen
                    (\exp ->
                        parseEnv envSource
                            |> Result.map (\dict -> ( exp, dict ))
                    )
    in
        test input
            (\_ ->
                case parseResult of
                    Ok ( exp, env_ ) ->
                        let
                            ( t, n, dep ) =
                                exp
                                    |> SimpleTyping.fromOriginalExp 0 Dict.empty
                                    |> logParseResult s

                            envTypes =
                                env_
                                    |> Dict.map
                                        (\id tExp ->
                                            SimpleTyping.fromTypeExp n Dict.empty tExp
                                                |> (\( t, _, _ ) -> t)
                                        )

                            env =
                                resolveDependencies envTypes dep
                        in
                            case evaluate env t of
                                Ok ( t, env ) ->
                                    let
                                        _ =
                                            Debug.log "" ""

                                        _ =
                                            Debug.log ("[ok]      " ++ input)
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

                                Err ( range, e ) ->
                                    let
                                        _ =
                                            Debug.log "" ""

                                        _ =
                                            Debug.log ("[err]     " ++ input) (toString e ++ " at " ++ Formatter.formatRange range)
                                    in
                                        if expected == "" then
                                            Expect.pass
                                        else
                                            toString e
                                                |> String.contains expected
                                                |> Expect.true (toString e ++ " should contain " ++ expected)

                    Err e ->
                        Expect.fail (ToyParser.formatError e)
            )


resolveDependencies : Dict String Type -> Dict String Int -> Env
resolveDependencies envTypes dep =
    dep
        |> Dict.toList
        |> List.filterMap
            (\( name, id ) ->
                envTypes
                    |> Dict.get name
                    |> Maybe.map
                        (\t ->
                            ( id, t )
                        )
            )
        |> Dict.fromList


parseEnv : Dict String String -> Result Parser.Error (Dict String ToyParser.TypeExp)
parseEnv envSource =
    envSource
        |> Dict.map (\_ s -> Parser.run ToyParser.typeExp s)
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
