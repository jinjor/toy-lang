module ParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Toy.Parser as ToyParser exposing (..)
import Toy.Formatter as Formatter
import Dict exposing (Dict)
import Parser


suite : Test
suite =
    describe "Parsing"
        [ describe "do-return"
            [ testParse "do\n  a = 1\n  b = 1\nreturn\nb" [ ( "Assignment", 2 ) ]
            ]
        ]


testParse : String -> List ( String, Int ) -> Test
testParse s expectedList =
    test s
        (\_ ->
            case Parser.run ToyParser.expression s of
                Ok exp ->
                    if checkAll expectedList s then
                        Expect.pass
                    else
                        Expect.fail (toString exp ++ " does not satisfy " ++ toString expectedList)

                Err e ->
                    Expect.fail (ToyParser.formatError e)
        )


checkAll : List ( String, Int ) -> String -> Bool
checkAll expectedList s =
    case expectedList of
        [] ->
            True

        ( name, count ) :: xs ->
            if List.length (String.indexes name s) == count then
                checkAll xs s
            else
                False
