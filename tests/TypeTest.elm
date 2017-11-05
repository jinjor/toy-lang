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


-- suite : Test
-- suite =
--     describe "Typing"
--         [ describe "eval"
--             [ testEval "a" [] ""
--             , testEval "\\a -> b" [] ""
--             , testEval "\\a -> b" [ "b" => "Int" ] ""
--             , testEval "\\a -> a" [] ""
--             , testEval "if a b c" [ "if" => "Bool -> a -> a -> a" ] ""
--             , testEval "a 1" [ "a" => "a" ] ""
--             , testEval "do a = (\\a -> a)\n return f (a 1) (a \"\")" [] ""
--             , testEval "(\\a -> f (a 1)) (\\a -> a)" [] ""
--             , testEval "(\\a -> f (a 1) (a \"\")) (\\a -> a)" [] ""
--             , testEval "(\\a -> f (a 1) (a \"\"))" [] ""
--             , testEval "do\na=1\nb=2\nreturn add a b" [] ""
--             , testEval "do\nb=1\nreturn append b" [ "append" => "String -> String" ] ""
--             , testEval "(\\b -> append b) 1" [ "append" => "String -> String" ] ""
--             , testEval "do\nb=1\nc=\"\"\nreturn plus b c" [ "plus" => "a -> a -> a" ] ""
--             ]
--         ]


suite : Test
suite =
    describe "Typing"
        [ describe "eval"
            [ testEval "a" [ "a" => "Int" ] (Ok "Int")
            , testEval "f" [ "f" => "Int -> String" ] (Ok "(Int -> String)")
            , testEval "a 1" [ "a" => "Int" ] (Err "Argument")
            , testEval "a 1" [ "a" => "Int -> String" ] (Ok "String")
            , testEval "a 1" [ "a" => "Int -> String -> Bool" ] (Ok "(String -> Bool)")
            , testEval "a 1" [ "a" => "String -> Int" ] (Err "Mismatch")
            , testEval "a 1" [ "a" => "a -> a" ] (Ok "Int")
            , testEval "a 1" [ "a" => "a -> a -> a" ] (Ok "(Int -> Int)")
            , testEval "f \"\" \"\"" [ "f" => "a -> a -> a" ] (Ok "String")
            , testEval "f 0 \"\"" [ "f" => "a -> a -> a" ] (Err "Mismatch")
            , testEval "f a \"\"" [ "f" => "a -> a -> a", "a" => "String" ] (Ok "String")
            , testEval "f a \"\"" [ "f" => "a -> a -> a", "a" => "Int" ] (Err "Mismatch")
            , testEval "f \"\" a" [ "f" => "a -> a -> a", "a" => "Int" ] (Err "Mismatch")
              -- Ok?
            , testEval "f \"\" a" [ "f" => "a -> a -> a" ] (Ok "String")
              -- Ok?
            , testEval "f a \"\"" [ "f" => "a -> a -> a" ] (Ok "String")
            , testEval "f a" [ "f" => "a -> a", "a" => "Int -> String" ] (Ok "(Int -> String)")
            , testEval "f a 1" [ "f" => "a -> a", "a" => "Int -> String" ] (Ok "String")
            , testEval "f (a 1) (a \"\")" [ "a" => "a -> a", "f" => "b -> c -> b -> c" ] (Ok "(Int -> String)")
            , testEval "(\\a -> \"\") 1" [] (Ok "String")
            , testEval "(\\a -> a) 1" [] (Ok "Int")
            , testEval "(\\a -> a) 1" [ "a" => "String" ] (Ok "Int")
            , testEval "(\\a -> f a)" [ "f" => "Int -> String" ] (Ok "(Int -> String)")
            , testEval "(\\a -> a) (f 1)" [ "f" => "Int -> Int" ] (Ok "Int")
            , testEval "(\\a -> a 1) (\\a -> a)" [] (Ok "Int")
            , testEval "(\\a -> f (a 1) (a \"\")) (\\a -> a)" [ "f" => "b -> c -> b -> c" ] (Ok "(Int -> String)")
            , testEval "do\na=1\na=\"\"\nreturn a" [] (Ok "String")
            , testEval "do\na=1\nb=a\nb=b\nreturn b" [] (Ok "Int")
            , testEval "map toString"
                [ "map" => "(a -> b) -> a -> b"
                , "toString" => "Int -> String"
                ]
                (Ok "(Int -> String)")
            , testEval "map 1"
                [ "map" => "(a -> b) -> a -> b"
                ]
                (Err "Few")
            , testEval "map a"
                [ "map" => "(a -> b) -> a -> b"
                , "a" => "Int -> String -> Bool"
                ]
                (Ok "(Int -> (String -> Bool))")
            , testEval "do\nf = \\a -> a\nreturn g (f 1) (f \"\")"
                [ "g" => "Int -> String -> Bool" ]
                (Ok "Bool")
            , testEval "a 1" [ "a" => "a -> A a" ] (Ok "A Int")
            , testEval "map toString list"
                [ "map" => "(a -> b) -> List a -> List b"
                , "toString" => "Int -> String"
                , "list" => "List Int"
                ]
                (Ok "List String")
            , testEval "map toString (cons 1 nil)"
                [ "map" => "(a -> b) -> List a -> List b"
                , "toString" => "Int -> String"
                , "cons" => "a -> List a -> List a"
                , "nil" => "List a"
                ]
                (Ok "List String")
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


testEval : String -> List ( String, String ) -> Result String String -> Test
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
                                    case expected of
                                        Ok expected ->
                                            Expect.equal expected (formatType t)

                                        _ ->
                                            Expect.fail "unexpectedy failed"

                                Err ( range, e ) ->
                                    -- let
                                    --     _ =
                                    --         Debug.log "" ""
                                    --
                                    --     _ =
                                    --         Debug.log ("[err]     " ++ input) (toString e ++ " at " ++ Formatter.formatRange range)
                                    -- in
                                    case expected of
                                        Ok _ ->
                                            Expect.fail "unexpectedy failed"

                                        Err expected ->
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
