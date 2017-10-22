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
    | Let Identifier Expression Expression


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
                [ lazy (\_ -> assignment)
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
            |= lazy (\_ -> expression)


identifier : Parser Identifier
identifier =
    inContext "identifier" <|
        source <|
            ignore (Exactly 1) Char.isLower
                |. ignore zeroOrMore (\c -> Char.isLower c || Char.isUpper c)


expression : Parser Expression
expression =
    inContext "expression" <|
        succeed makeCall
            |= singleExpression
            |. spaces
            |= lazy (\_ -> functionTail)


makeCall : Expression -> List Expression -> Expression
makeCall head tail =
    case tail of
        [] ->
            head

        x :: xs ->
            makeCall (Call head x) xs


singleExpression : Parser Expression
singleExpression =
    inContext "single expression" <|
        oneOf
            [ number
            , string
            , lazy (\_ -> lambda)
            , lazy (\_ -> doReturn)
            , ref
            , succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
                |. symbol ")"
            ]


functionTail : Parser (List Expression)
functionTail =
    inContext "function tail" <|
        oneOf
            [ succeed (::)
                |= lazy (\_ -> singleExpression)
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


doReturn : Parser Expression
doReturn =
    inContext "do return" <|
        succeed makeLet
            |. keyword "do"
            |. spaces
            |= lazy (\_ -> onelineStatementsUntilIn)
            |. spaces
            |= lazy (\_ -> expression)


makeLet : List ( Identifier, Expression ) -> Expression -> Expression
makeLet assignments exp =
    case assignments of
        [] ->
            exp

        ( left, right ) :: xs ->
            Let left right (makeLet xs exp)


onelineStatementsUntilIn : Parser (List ( Identifier, Expression ))
onelineStatementsUntilIn =
    inContext "oneline statements for debug" <|
        oneOf
            [ succeed []
                |. keyword "return"
            , succeed (::)
                |= lazy (\_ -> assignment_)
                |. spaces
                |. symbol ";"
                |. spaces
                |= lazy (\_ -> onelineStatementsUntilIn)
            ]


assignment_ : Parser ( Identifier, Expression )
assignment_ =
    inContext "assignment for debug" <|
        succeed (,)
            |= identifier
            |. spaces
            |. symbol "="
            |. spaces
            |= lazy (\_ -> expression)


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
            |. symbol "\""
            |= (source <|
                    ignore zeroOrMore (\c -> c /= '"')
               )
            |. symbol "\""


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


spaces1 : Parser ()
spaces1 =
    ignore oneOrMore (\c -> c == ' ')
