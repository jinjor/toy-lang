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
    | TypeSignature Identifier TypeExp


type alias Identifier =
    String


type TypeExp
    = ArrowType TypeExp (Maybe TypeExp)
    | AtomType String


type Expression
    = NumberLiteral String
    | StringLiteral String
    | Ref Identifier (List (Positioned Expression))


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
        succeed (\typeExp id -> TypeSignature id typeExp)
            |. symbol ":"
            |. spaces
            |= typeExp


typeExp : Parser TypeExp
typeExp =
    inContext "type names" <|
        (oneOf
            [ succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> typeExp)
                |. spaces
                |. symbol ")"
            , map AtomType typeName
            ]
            |> andThen
                (\head ->
                    succeed (ArrowType head)
                        |. spaces
                        |= oneOf
                            [ succeed Just
                                |. symbol "->"
                                |. spaces
                                |= lazy (\_ -> typeExp)
                            , succeed Nothing
                            ]
                )
        )


typeName : Parser String
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
        oneOf
            [ lazy (\_ -> ref)
            , positioned number
            ]


ref : Parser (Positioned Expression)
ref =
    inContext "ref" <|
        positioned <|
            succeed Ref
                |= identifier
                |. spaces
                |= lazy (\_ -> functionTail)


functionTail : Parser (List (Positioned Expression))
functionTail =
    inContext "function tail" <|
        oneOf
            [ succeed (::)
                |= lazy (\_ -> expression)
                |. spaces
                |= lazy (\_ -> functionTail)
            , succeed []
            ]


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
