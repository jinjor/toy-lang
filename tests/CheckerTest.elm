module CheckerTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Toy.Parser as ToyParser
import Toy.Checker as ToyChecker
import Parser


suite : Test
suite =
    describe "Checker"
        [ describe "errors"
            [ testCheck "empty" "" []
            , testCheck "undefined variable" "a=b" [ "VariableNotDefined" ]
              -- , testCheck "duplicated difinition" "a=1\na=1" [ "DifinitionDuplicated" ]
              -- , testCheck "duplicated type difinition" "a:Int\na:Int" [ "DifinitionDuplicated" ]
            , testCheck "unknown type" "a:Unknown" [ "TypeNotDefined" ]
            , testCheck "too few type arguments" "a:List" [ "TooFewTypeArguments" ]
            , testCheck "too many type arguments" "a:List Int Int" [ "TooManyTypeArguments" ]
            , testCheck "type annotation mismatch" "a:String\na=1" [ "Mismatch" ]
              -- , testCheck "multiple errors"
              --     "a=b\na=c\nb:Unknown\nb:Int"
              --     [ "VariableNotDefined", "DifinitionDuplicated", "TypeNotDefined" ]
            ]
          -- , describe "interfaces"
          --     []
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
                                |> Debug.log "errors"

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
