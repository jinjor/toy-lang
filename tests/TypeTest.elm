module TypeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Toy.Type exposing (..)
import Toy.Typing as Typing exposing (..)
import Toy.Parser as ToyParser
import Toy.Checker as Checker
import Toy.Formatter as Formatter exposing (formatType)
import Toy.Debug as ToyDebug
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
        [ describe "match"
            [ testMatch "Int" "Int" (Ok 0)
            , testMatch "String" "Int" (Err "Mismatch")
            , testMatch "a" "Int" (Ok 1)
            , testMatch "Int" "a" (Ok 1)
            , testMatch "List Int" "List Int" (Ok 0)
            , testMatch "List Int" "List Bool" (Err "Mismatch")
            , testMatch "List a" "List Bool" (Ok 1)
            , testMatch "Dict Int String" "Dict Int String" (Ok 0)
            , testMatch "Dict Int String" "Dict Int Bool" (Err "Mismatch")
            , testMatch "Dict Int String" "Dict Bool String" (Err "Mismatch")
            , testMatch "Dict a String" "Dict Int String" (Ok 1)
            , testMatch "Dict Int a" "Dict Int String" (Ok 1)
            , testMatch "Dict a a" "Dict Int Int" (Ok 1)
            , testMatch "Int -> String" "Int -> String" (Ok 0)
            , testMatch "Int -> String" "Int" (Err "Few")
              -- , testMatch "Int -> String" "Int -> String -> Bool" (Err "Many")
            , testMatch "Int" "Int -> String" (Err "Mismatch")
            , testMatch "a" "Int -> String" (Ok 1)
            , testMatch "Int -> a" "Int -> String -> Bool" (Ok 1)
            , testMatch "a -> b" "Int -> String" (Ok 2)
            , testMatch "a -> a" "Int -> Int" (Ok 1)
              -- , testMatch "a -> a" "Int -> String" (Err "Mismatch")
            ]
        , describe "eval"
            [ testEval "a" [ "a" => "Int" ] (Ok "Int")
            , testEval "f" [ "f" => "Int -> String" ] (Ok "(Int -> String)")
            , testEval "a 1" [ "a" => "Int" ] (Err "Argument")
            , testEval "a 1" [ "a" => "Int -> String" ] (Ok "String")
            , testEval "a 1" [ "a" => "Int -> String -> Bool" ] (Ok "(String -> Bool)")
            , testEval "a 1" [ "a" => "String -> Int" ] (Err "Mismatch")
            , testEval "a 1" [ "a" => "a -> a" ] (Ok "Int")
            , testEval "a 1" [ "a" => "a -> a -> a" ] (Ok "(Int -> Int)")
            , testEval "a (\\a -> a)" [ "a" => "Int" ] (Err "Many")
            , testEval "a (\\a -> a)" [ "a" => "Int -> String" ] (Err "Mismatch")
            , testEval "a (\\a -> a)" [ "a" => "a -> String" ] (Ok "String")
            , testEval "a b" [ "a" => "(a -> a) -> (a -> a)", "b" => "Int -> Int" ] (Ok "(Int -> Int)")
            , testEval "a b" [ "a" => "(a -> b) -> b", "b" => "Int -> String -> Bool" ] (Ok "(String -> Bool)")
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


(/>) a b =
    b
infixl 1 />


testMatch : String -> String -> Result String Int -> Test
testMatch left right expected =
    test (left ++ " <== " ++ right)
        (\_ ->
            case ( Parser.run (ToyParser.typeExp 1) left, Parser.run (ToyParser.typeExp 1) right ) of
                ( Ok left, Ok right ) ->
                    let
                        ( leftType, st ) =
                            Typing.fromTypeExp
                                (Typing.initFromTypeExpState 0)
                                left

                        ( rightType, _ ) =
                            Typing.fromTypeExp
                                (Typing.initFromTypeExpState st.n)
                                right
                    in
                        case match Dict.empty leftType rightType of
                            Ok env ->
                                case expected of
                                    Ok i ->
                                        Expect.equal i (Dict.size env)

                                    Err e ->
                                        Expect.fail ("unexpectedly succeeded: " ++ toString env)

                            Err e ->
                                case expected of
                                    Ok _ ->
                                        Expect.fail ("unexpectedly failed: " ++ toString e)

                                    Err s ->
                                        toString e
                                            |> String.contains s
                                            |> Expect.true (toString e ++ " should contain " ++ s)

                e ->
                    Expect.fail (toString e)
        )


logParseResult : String -> ( Type, Int, Dependency ) -> ( Type, Int, Dependency )
logParseResult s (( t, _, dep ) as r) =
    Debug.log ("[parsed]  " ++ s)
        (formatTypeWithDependency t dep)
        /> r


formatTypeWithDependency : Type -> Dependency -> String
formatTypeWithDependency t dep =
    formatType t
        ++ (if Dict.isEmpty dep then
                ""
            else
                " with " ++ ToyDebug.formatDict identity (Tuple.second >> toString) dep
           )


testEval : String -> List ( String, String ) -> Result String String -> Test
testEval s envSource_ expected =
    let
        envSource =
            Dict.fromList envSource_

        input =
            (s ++ " with " ++ ToyDebug.formatDict identity identity envSource)

        parseResult =
            Parser.run (ToyParser.expression 1) s
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
                                    ()
                                        /> Debug.log "" ""
                                        /> Debug.log ("[ok]      " ++ input) (formatEvalResult t env)
                                        /> case expected of
                                            Ok expected ->
                                                Expect.equal expected (formatType t)

                                            _ ->
                                                Expect.fail "unexpectedy failed"

                                Err ( range, e ) ->
                                    ()
                                        /> Debug.log "" ""
                                        /> Debug.log
                                            ("[err]     " ++ input)
                                            (toString e ++ " at " ++ Formatter.formatRange range)
                                        /> case expected of
                                            Ok _ ->
                                                Expect.fail "unexpectedy failed"

                                            Err expected ->
                                                toString e
                                                    |> String.contains expected
                                                    |> Expect.true (toString e ++ " should contain " ++ expected)

                    Err e ->
                        Expect.fail (ToyParser.formatError e)
            )


formatEvalResult : Type -> Env -> String
formatEvalResult t env =
    formatType t
        ++ (if Dict.isEmpty env then
                ""
            else
                " with " ++ ToyDebug.formatDict toString formatType env
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
