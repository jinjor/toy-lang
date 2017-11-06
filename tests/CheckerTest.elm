module CheckerTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Toy.Parser as ToyParser
import Toy.Checker as ToyChecker
import Parser


suite : Test
suite =
    describe "Checker"
        [ describe "eval"
            [ testCheck "empty" "" []
            ]
        ]


testCheck : String -> String -> List String -> Test
testCheck name code expectedErrors =
    test name <|
        \_ ->
            case Parser.run (ToyParser.module_) code of
                Ok module_ ->
                    let
                        ( errors, interfaces, implementations ) =
                            ToyChecker.check module_

                        errStr =
                            toString errors

                        ok =
                            List.all
                                (\expectedError -> String.contains expectedError errStr)
                                expectedErrors
                    in
                        if ok then
                            Expect.pass
                        else
                            Expect.fail "not satisfied"

                Err e ->
                    Expect.fail (toString e)
