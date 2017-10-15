module Toy.SimpleParser exposing (..)

import Char
import Parser exposing (..)
import Parser.LowLevel exposing (..)


type Module
    = Module (List Statement)


type Statement
    = Assignment Identifier Expression
    | TypeSignature Identifier TypeExp


type alias Identifier =
    String


type TypeExp
    = ArrowType TypeExp TypeExp
    | TypeValue TypeConstructor (List TypeExp)
    | TypeVar String


type alias TypeConstructor =
    String


type Expression
    = IntLiteral String
    | StringLiteral String
    | Ref Identifier
    | Call Expression Expression
    | Lambda String Expression
    | Let (List Statement) Expression


type Patterns
    = Patterns Pattern (Maybe Patterns)


type alias Pattern =
    String


module_ : Parser Module
module_ =
    inContext "module" <|
        succeed Module
            |= statements ""


statements : String -> Parser (List Statement)
statements indent =
    inContext "statements" <|
        succeed identity
            |. repeat zeroOrMore emptyLine
            |= oneOf
                [ symbol indent
                    |> andThen
                        (\_ ->
                            succeed (::)
                                |= statement
                                |. spaces
                                |. symbol "\n"
                                |= statements indent
                        )
                , succeed []
                ]


emptyLine : Parser ()
emptyLine =
    ignore zeroOrMore (\c -> c == ' ')
        |. symbol "\n"


statement : Parser Statement
statement =
    inContext "statement" <|
        succeed (\id f -> f id)
            |= identifier
            |. spaces
            |= oneOf
                [ assignment
                , typeSignature
                ]


typeSignature : Parser (Identifier -> Statement)
typeSignature =
    inContext "type signature" <|
        succeed (\typeExp id -> TypeSignature id typeExp)
            |. symbol ":"
            |. spaces
            |= typeExp


typeExp : Parser TypeExp
typeExp =
    inContext "type expression" <|
        (lazy (\_ -> singleTypeExp)
            |> andThen
                (\head ->
                    succeed identity
                        |. spaces
                        |= oneOf
                            [ succeed (ArrowType head)
                                |. symbol "->"
                                |. spaces
                                |= lazy (\_ -> typeExp)
                            , succeed head
                            ]
                )
        )


singleTypeExp : Parser TypeExp
singleTypeExp =
    inContext "single type expression" <|
        oneOf
            [ succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> typeExp)
                |. spaces
                |. symbol ")"
            , lazy (\_ -> typeValue)
            , typeVariable
            ]


typeValue : Parser TypeExp
typeValue =
    inContext "type value" <|
        succeed TypeValue
            |= typeConstructor
            |. spaces
            |= lazy (\_ -> typeArguments)


typeArguments : Parser (List TypeExp)
typeArguments =
    inContext "type arguments" <|
        oneOf
            [ succeed (::)
                |= lazy (\_ -> singleTypeExp)
                |. spaces
                |= lazy (\_ -> typeArguments)
            , succeed []
            ]


typeConstructor : Parser String
typeConstructor =
    inContext "type constructor" <|
        source <|
            ignore (Exactly 1) Char.isUpper
                |. ignore zeroOrMore (\c -> Char.isLower c || Char.isUpper c)


typeVariable : Parser TypeExp
typeVariable =
    inContext "type variable" <|
        map TypeVar <|
            source <|
                ignore (Exactly 1) Char.isLower
                    |. ignore zeroOrMore (\c -> Char.isLower c || Char.isUpper c)


assignment : Parser (Identifier -> Statement)
assignment =
    inContext "assignment" <|
        succeed (\exp id -> Assignment id exp)
            |. symbol "="
            |. spaces
            |= expression


identifier : Parser Identifier
identifier =
    inContext "identifier" <|
        source <|
            ignore (Exactly 1) Char.isLower
                |. ignore zeroOrMore (\c -> Char.isLower c || Char.isUpper c)


expression : Parser Expression
expression =
    inContext "expression" <|
        oneOf
            [ number
            , string
            , lazy (\_ -> lambda)
            , succeed makeCall
                |= ref
                |. spaces
                |= lazy (\_ -> functionTail)
            , lazy (\_ -> letin)
            , succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
                |. symbol ")"
            ]


makeCall : Expression -> List Expression -> Expression
makeCall head tail =
    case tail of
        [] ->
            head

        x :: xs ->
            makeCall (Call head x) xs


functionTail : Parser (List Expression)
functionTail =
    inContext "function tail" <|
        oneOf
            [ succeed (::)
                |= lazy (\_ -> expression)
                |. spaces
                |= lazy (\_ -> functionTail)
            , succeed []
            ]


lambda : Parser Expression
lambda =
    inContext "lambda" <|
        succeed Lambda
            |. symbol "\\"
            |= identifier
            |. spaces
            |. symbol "->"
            |. spaces
            |= lazy (\_ -> expression)


patterns : Parser Patterns
patterns =
    inContext "patterns" <|
        succeed Patterns
            |= pattern
            |. spaces
            |= oneOf
                [ succeed Just
                    |= lazy (\_ -> patterns)
                , succeed Nothing
                ]


pattern : Parser Pattern
pattern =
    inContext "pattern" <|
        source <|
            ignore (Exactly 1) Char.isLower
                |. ignore zeroOrMore (\c -> Char.isLower c || Char.isUpper c)


ref : Parser Expression
ref =
    inContext "ref" <|
        succeed Ref
            |= identifier


letin : Parser Expression
letin =
    inContext "let in" <|
        succeed Let
            |. keyword "let"
            |. spaces
            |= onelineStatementsUntilIn
            |. spaces
            |= lazy (\_ -> expression)


onelineStatementsUntilIn : Parser (List Statement)
onelineStatementsUntilIn =
    inContext "oneline statements for debug" <|
        oneOf
            [ succeed []
                |. keyword "in"
            , succeed (::)
                |= statement
                |. spaces
                |. symbol ";"
                |= lazy (\_ -> onelineStatementsUntilIn)
            , succeed []
            ]


number : Parser Expression
number =
    inContext "number" <|
        succeed IntLiteral
            |= oneOf
                [ map toString int
                , map toString float
                ]


string : Parser Expression
string =
    inContext "string" <|
        succeed StringLiteral
            |. symbol "'"
            |= (source <|
                    ignore zeroOrMore (\c -> c /= '\'')
               )
            |. symbol "'"


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


spaces1 : Parser ()
spaces1 =
    ignore oneOrMore (\c -> c == ' ')


formatError : Parser.Error -> String
formatError e =
    toString e.row ++ ":" ++ toString e.col ++ " " ++ formatProblem e.problem


formatProblem : Parser.Problem -> String
formatProblem problem =
    case problem of
        BadOneOf problems ->
            problems
                |> List.map formatProblem
                |> String.join ", "

        BadInt ->
            "not an integer"

        BadFloat ->
            "not a float"

        BadRepeat ->
            "internal error"

        ExpectingEnd ->
            "expecting end"

        ExpectingSymbol symbol ->
            "expecting symbol " ++ toString symbol

        ExpectingKeyword keyword ->
            "expecting keyword " ++ toString keyword

        ExpectingVariable ->
            "expecting variable"

        ExpectingClosing s ->
            "expecting closing " ++ toString s

        Fail s ->
            s
