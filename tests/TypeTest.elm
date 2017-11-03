module TypeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Toy.Type exposing (..)
import Toy.Typing as Typing exposing (..)
import Toy.Parser as ToyParser
import Toy.Checker as Checker
import Toy.Formatter as Formatter exposing (formatType)
import Dict exposing (Dict)
import Parser


suite : Test
suite =
    describe "Typing"
        [ describe "eval"
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
            , testEval "do a = (\\a -> a)\n return f (a 1) (a \"\")" [] ""
            , testEval "(\\a -> a 1) (\\a -> a)" [] "Int"
            , testEval "(\\a -> f (a 1)) (\\a -> a)" [] ""
            , testEval "(\\a -> f (a 1) (a \"\")) (\\a -> a)" [] ""
            , testEval "(\\a -> f (a 1) (a \"\"))" [] ""
            , testEval "f (a 1) (a \"\")" [ "a" => "a -> a", "f" => "b -> c -> b -> c" ] "(Int -> String)"
            , testEval "do\na=1\nb=2\nreturn add a b" [] ""
            , testEval "do\na=1\na=\"\"\nreturn a" [] "String"
            , testEval "do\na=1\nb=a\nb=b\nreturn b" [] "Int"
            , testEval "do\nb=1\nreturn append b" [ "append" => "String -> String" ] ""
            , testEval "(\\b -> append b) 1" [ "append" => "String -> String" ] ""
            , testEval "do\nb=1\nc=\"\"\nreturn plus b c" [ "plus" => "a -> a -> a" ] ""
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
            , testEval "do\nf = \\a -> a\nreturn g (f 1) (f \"\")"
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


logParseResult : String -> ( Type, Int, Dependency ) -> ( Type, Int, Dependency )
logParseResult s (( t, _, dep ) as r) =
    -- let
    --     _ =
    --         Debug.log ("[parsed]  " ++ s)
    --             (formatType t
    --                 ++ (if Dict.isEmpty dep then
    --                         ""
    --                     else
    --                         " with " ++ formatDict identity toString dep
    --                    )
    --             )
    -- in
    r


testEval : String -> List ( String, String ) -> String -> Test
testEval s envSource_ expected =
    let
        envSource =
            Dict.fromList envSource_

        input =
            (s ++ " with " ++ formatDict identity identity envSource)

        parseResult =
            Parser.run (ToyParser.expression 0) s
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
                                    |> Typing.fromExp 0 Dict.empty
                                    |> logParseResult s

                            envTypes =
                                env_
                                    |> Dict.map
                                        (\id tExp ->
                                            Typing.fromTypeExp
                                                (Typing.initFromTypeExpState n)
                                                tExp
                                                |> Tuple.first
                                        )

                            ( _, env ) =
                                Checker.resolveDependencies envTypes dep
                        in
                            case evaluate env t of
                                Ok ( t, env ) ->
                                    -- let
                                    --     _ =
                                    --         Debug.log "" ""
                                    --
                                    --     _ =
                                    --         Debug.log ("[ok]      " ++ input)
                                    --             (formatType t
                                    --                 ++ (if Dict.isEmpty env then
                                    --                         ""
                                    --                     else
                                    --                         " with " ++ formatDict toString formatType env
                                    --                    )
                                    --             )
                                    -- in
                                    if expected == "" then
                                        Expect.pass
                                    else
                                        Expect.equal expected (formatType t)

                                Err ( range, e ) ->
                                    -- let
                                    --     _ =
                                    --         Debug.log "" ""
                                    --
                                    --     _ =
                                    --         Debug.log ("[err]     " ++ input) (toString e ++ " at " ++ Formatter.formatRange range)
                                    -- in
                                    if expected == "" then
                                        Expect.pass
                                    else
                                        toString e
                                            |> String.contains expected
                                            |> Expect.true (toString e ++ " should contain " ++ expected)

                    Err e ->
                        Expect.fail (ToyParser.formatError e)
            )


parseEnv : Dict String String -> Result Parser.Error (Dict String ToyParser.TypeExp)
parseEnv envSource =
    envSource
        |> Dict.map (\_ s -> Parser.run (ToyParser.typeExp 1) s)
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
