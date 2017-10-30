module ParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Toy.Parser as ToyParser exposing (..)
import Toy.Formatter as Formatter
import Dict exposing (Dict)
import Parser exposing (Parser)


suite : Test
suite =
    describe "Parsing"
        [ describe "number"
            [ testParse number "1" [ ( "Int", 1 ) ]
            ]
        , describe "expression"
            [ testParse expression "( 1 )" [ ( "Int", 1 ) ]
            , testParse expression "(\n1\n)" [ ( "Int", 1 ) ]
            ]
        , describe "do-return"
            [ testParse expression "do a = 1 return a" [ ( "Assignment", 1 ) ]
            , testParse expression "do a = 1\n   b = 1\nreturn\nb" [ ( "Assignment", 2 ) ]
            ]
        , describe "statement"
            [ testParse statement "a:Int" [ ( "TypeSignature", 1 ) ]
            , testParse statement "a : Int " [ ( "TypeSignature", 1 ) ]
            , testParse statement "a=1" [ ( "Assignment", 1 ) ]
            , testParse statement "a = 1 " [ ( "Assignment", 1 ) ]
            ]
        ]


testParseFailure : Parser a -> String -> Test
testParseFailure parser s =
    test s
        (\_ ->
            case Parser.run parser s of
                Ok exp ->
                    Expect.fail ("unexpectedly succeeded: " ++ toString exp)

                Err e ->
                    Expect.pass
        )


testParse : Parser a -> String -> List ( String, Int ) -> Test
testParse parser s expectedList =
    test s
        (\_ ->
            case Parser.run parser s of
                Ok exp ->
                    if checkAll expectedList (toString exp) then
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
