module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Toy.SimpleTyping as SimpleTyping exposing (..)
import Toy.SimpleParser as SimpleParser
import Dict
import Parser


suite : Test
suite =
    describe "SimpleTyping"
        [ describe "calc"
            [ test "01" <| testCalc "1"
            , test "02" <| testCalc "''"
            , test "03" <| testCalc "a"
            , test "04" <| testCalc "f 1"
            , test "05" <| testCalc "f 1 ''"
            , test "06" <| testCalc "f a"
            , test "07" <| testCalc "\\a -> 1"
            , test "08" <| testCalc "\\a -> a"
            , test "09" <| testCalc "\\a -> increment a"
            , test "10" <| testCalc "\\a -> add a 1"
            , test "11" <| testCalc "\\a -> \\b -> 1"
            , test "12" <| testCalc "\\a -> \\b -> a"
            , test "13" <| testCalc "let a = 1 in a"
            , test "14" <| testCalc "let a = (\\a -> a) in a"
            ]
          {-
             01: a -- env={}
             02: \a -> b -- env={}
             03: \a -> b -- env={ b: Int }
             04: \a -> a -- env={}
             05: (\a -> "") 1 -- env={}
             06: (\a -> a) 1 -- env={}
             07: (\a -> f a) -- env={ f: Int -> String }
             08: if a b c -- env={ if: Bool -> a -> a -> a }
             09: a 1 -- env={ a: Int }
             10: a 1 -- env={ a: a }
             11: a 1 -- env={ a: Int -> String }
             12: a 1 -- env={ a: String -> Int }
             13: a 1 -- env={ a: a -> a }
          -}
        , describe "eval"
            [ test "01" <| testEval (TypeVar 1) Dict.empty
            , test "02" <| testEval (TypeArrow (TypeVar 1) (TypeVar 2)) Dict.empty
            , test "03" <| testEval (TypeArrow (TypeVar 1) (TypeVar 2)) (Dict.singleton 2 (TypeValue "Int"))
            , test "04" <| testEval (TypeArrow (TypeVar 1) (TypeVar 1)) Dict.empty
            , test "05" <| testEval (TypeApply (TypeArrow (TypeVar 1) (TypeValue "String")) (TypeValue "Int")) Dict.empty
            , test "06" <| testEval (TypeApply (TypeArrow (TypeVar 1) (TypeVar 1)) (TypeValue "Int")) Dict.empty
            , test "07" <|
                testEval
                    (TypeArrow (TypeVar 1) (TypeApply (TypeVar 2) (TypeVar 1)))
                    (Dict.singleton 2 (TypeArrow (TypeValue "Int") (TypeValue "String")))
            , test "08" <|
                testEval
                    (TypeApply (TypeApply (TypeApply (TypeVar 1) (TypeVar 2)) (TypeVar 3)) (TypeVar 4))
                    (Dict.singleton 1 (TypeArrow (TypeValue "Bool") (TypeArrow (TypeVar 5) (TypeArrow (TypeVar 5) (TypeVar 5)))))
            , test "09" <|
                testEval
                    (TypeApply (TypeVar 1) (TypeValue "Int"))
                    (Dict.singleton 1 (TypeValue "Int"))
            , test "10" <|
                testEval
                    (TypeApply (TypeVar 1) (TypeValue "Int"))
                    (Dict.singleton 1 (TypeVar 2))
            , test "11" <|
                testEval
                    (TypeApply (TypeVar 1) (TypeValue "Int"))
                    (Dict.singleton 1 (TypeArrow (TypeValue "Int") (TypeValue "String")))
            , test "12" <|
                testEval
                    (TypeApply (TypeVar 1) (TypeValue "Int"))
                    (Dict.singleton 1 (TypeArrow (TypeValue "String") (TypeValue "Int")))
            , test "13" <|
                testEval
                    (TypeApply (TypeVar 1) (TypeValue "Int"))
                    (Dict.singleton 1 (TypeArrow (TypeVar 1) (TypeVar 1)))
            ]
        ]


testCalc : String -> () -> Expectation
testCalc s _ =
    case Parser.run SimpleParser.expression s of
        Ok exp ->
            exp
                |> SimpleTyping.calc 0 Dict.empty Dict.empty
                |> (\( t, _, env ) -> ( t, env ))
                |> (\( t, dep ) -> Debug.log s (formatType t))
                |> always Expect.pass

        Err e ->
            Expect.fail (SimpleParser.formatError e)



-- testEval : SimpleTyping.Type -> SimpleTyping.Env -> () -> Expectation
-- testEval s e _ =
--     case Parser.run SimpleParser.expression s of
--         Ok exp ->
--             let
--                 ( t, _, env ) =
--                     exp
--                         |> SimpleTyping.calc 0 Dict.empty Dict.empty
--             in
--                 t
--                     |> evaluate e
--                     |> Result.map (Tuple.first >> formatType)
--                     |> Debug.log s
--                     |> always Expect.pass
--
--         Err e ->
--             Expect.fail (SimpleParser.formatError e)


testEval : SimpleTyping.Type -> SimpleTyping.Env -> () -> Expectation
testEval t e _ =
    t
        |> evaluate e
        |> Result.map (Tuple.first >> formatType)
        |> Debug.log "test-eval"
        |> always Expect.pass
