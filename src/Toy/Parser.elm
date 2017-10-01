module Toy.Parser exposing (..)

import Char
import Parser exposing (..)
import Parser.LowLevel exposing (..)


type Module
    = Module (List (Positioned Statement))


type alias Position =
    ( Int, Int )


type alias Positioned a =
    { start : Position
    , end : Position
    , content : a
    }


type Statement
    = Assignment Identifier (Positioned Expression)
    | TypeSignature Identifier TypeName


type alias Identifier =
    String


type alias TypeName =
    String


type Expression
    = NumberLiteral String
    | StringLiteral String
    | Ref Identifier


module_ : Parser Module
module_ =
    inContext "module" <|
        succeed Module
            |= statements ""


statements : String -> Parser (List (Positioned Statement))
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


statement : Parser (Positioned Statement)
statement =
    inContext "statement" <|
        positioned <|
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
        succeed (\typeName id -> TypeSignature id typeName)
            |. symbol ":"
            |. spaces
            |= typeName


typeName : Parser TypeName
typeName =
    inContext "type name" <|
        source <|
            ignore (Exactly 1) Char.isUpper
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


expression : Parser (Positioned Expression)
expression =
    inContext "expression" <|
        positioned <|
            oneOf
                [ ref
                , number
                ]


ref : Parser Expression
ref =
    inContext "ref" <|
        map Ref identifier


number : Parser Expression
number =
    inContext "number" <|
        succeed NumberLiteral
            |= oneOf
                [ map toString int
                , map toString float
                ]


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


positioned : Parser a -> Parser (Positioned a)
positioned parser =
    succeed (\start a end -> Positioned start end a)
        |= getPosition
        |= parser
        |= getPosition
