module ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Toy.Parser as ToyParser exposing (..)
import Toy.Formatter as Formatter
import Dict exposing (Dict)
import Parser exposing (Parser)


suite : Test
suite =
    describe "Parsing"
        [ describe "int"
            [ testParse intLiteral "1" [ ( "Int", 1 ) ]
            ]
        , describe "string"
            [ testParse stringLiteral "\"foo\"" [ ( "foo", 1 ) ]
            ]
        , describe "list"
            [ testParse (listLiteral 1) "[]" [ ( "List", 1 ) ]
            , testParse (listLiteral 1) "[ [ ] ]" [ ( "List", 2 ) ]
            , testParse (listLiteral 1) "[ foo ]" [ ( "List", 1 ), ( "foo", 1 ) ]
            , testParse (listLiteral 1) "[ foo, bar ]" [ ( "List", 1 ), ( "foo", 1 ), ( "bar", 1 ) ]
            ]
        , describe "ref"
            [ testParse ref "foo" [ ( "Ref", 1 ), ( "foo", 1 ) ]
            , testParseFailure ref "return"
            ]
        , describe "lambda"
            [ testParse (expression 1) "\\a->1" [ ( "Int", 1 ) ]
            , testParse (expression 1) "\\a->\n1" [ ( "Int", 1 ) ]
            , testParse (expression 1) "\\a\n->1" [ ( "Int", 1 ) ]
            , testParse (expression 1) "\\foo bar->1" [ ( "foo", 1 ), ( "bar", 1 ), ( "Int", 1 ) ]
            , testParse (expression 1) "\\foo\n bar\n ->\n 1" [ ( "foo", 1 ), ( "bar", 1 ), ( "Int", 1 ) ]
            ]
        , describe "parens"
            [ testParse (expression 1) "( 1 )" [ ( "Int", 1 ) ]
            , testParse (expression 1) "(\n1\n)" [ ( "Int", 1 ) ]
            ]
        , describe "call"
            [ testParse (expression 1) "a 1" [ ( "Call", 1 ), ( "Ref", 1 ), ( "Int", 1 ) ]
            , testParse (expression 1) "a\n 1" [ ( "Call", 1 ), ( "Ref", 1 ), ( "Int", 1 ) ]
            , testParse (expression 1) "a\n1" [ ( "Call", 0 ), ( "Ref", 1 ), ( "Int", 0 ) ]
            , testParse (expression 1) "foo does" [ ( "Call", 1 ), ( "foo", 1 ), ( "does", 1 ) ]
            , testParse (expression 1) "does foo" [ ( "Call", 1 ), ( "does", 1 ), ( "foo", 1 ) ]
            ]
        , describe "do-return"
            [ testParse (expression 1) "do return 1" [ ( "Int", 1 ) ]
            , testParse (expression 1) "doreturn 1" [ ( "doreturn", 1 ) ]
            , testParse (expression 1) "do a = 1 return a" [ ( "Let", 1 ) ]
            , testParse (expression 1) "do a = 1\n   b = 1\nreturn\nb" [ ( "Let", 2 ) ]
            , testParse (expression 1) "do a = 1\n   returns = 1\nreturn\nreturns" [ ( "Let", 2 ) ]
            ]
        , describe "type value"
            [ testParse (typeValue 1) "List Int" [ ( "List", 1 ), ( "Int", 1 ) ]
            , testParse (typeValue 1) "List\n Int" [ ( "List", 1 ), ( "Int", 1 ) ]
            , testParse (typeValue 1) "List\n foo" [ ( "List", 1 ), ( "foo", 1 ) ]
            , testParse (typeValue 1) "List\n foo\n bar" [ ( "List", 1 ), ( "foo", 1 ), ( "bar", 1 ) ]
            ]
        , describe "type expression"
            [ testParse (typeExp 1) "foo -> bar" [ ( "foo", 1 ), ( "bar", 1 ) ]
            , testParse (typeExp 1) "foo\n ->\n bar" [ ( "foo", 1 ), ( "bar", 1 ) ]
            , testParse (typeExp 1) "List\n foo\n ->\n bar" [ ( "List", 1 ), ( "foo", 1 ), ( "bar", 1 ) ]
            ]
        , describe "type signature"
            [ testParse (typeSignature) "a\n :\n Dict\n foo" [ ( "foo", 1 ) ]
            , testParse (typeSignature) "a\n :\n Dict\n foo\n bar\n ->\n baz" [ ( "foo", 1 ), ( "bar", 1 ), ( "baz", 1 ) ]
            ]
        , describe "statement"
            [ testParse (statement 1) "a:Int" [ ( "TypeSignature", 1 ) ]
            , testParse (statement 1) "a : Int " [ ( "TypeSignature", 1 ) ]
            , testParse (statement 1) "a\n :\n Int " [ ( "TypeSignature", 1 ) ]
            , testParse (statement 1) "a\n :\n Dict\n a\n b" [ ( "TypeSignature", 1 ) ]
            , testParse (statement 1) "a\n :\n Dict\n a\n b\n ->\n foo" [ ( "TypeSignature", 1 ), ( "foo", 1 ) ]
            , testParse (statement 1) "a\n :\n (\n a\n ->\n foo\n )" [ ( "TypeSignature", 1 ), ( "foo", 1 ) ]
            , testParse (statement 1) "a=1" [ ( "Assignment", 1 ) ]
            , testParse (statement 1) "a = 1 " [ ( "Assignment", 1 ) ]
            , testParse (statement 1) "a\n = 1 " [ ( "Assignment", 1 ) ]
            , testParse (statement 1) "a =\n 1 " [ ( "Assignment", 1 ) ]
            , testParse (statement 1) "a\n =\n 1 " [ ( "Assignment", 1 ) ]
            ]
        , describe "module"
            [ testParse (module_) "a=1\nb=2 " [ ( "Assignment", 2 ) ]
            , testParse (module_) "a:String\na=2 " [ ( "TypeSignature", 1 ), ( "Assignment", 1 ) ]
            , testParse (module_) "a=\\a -> a\nb=1 " [ ( "Assignment", 2 ) ]
            , testParse (module_) "\n\n\na=1\n\n\n\nb=3\n\n\n\n\n" [ ( "Assignment", 2 ) ]
            , testParseFailure (module_) "a=b=2"
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
