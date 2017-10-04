module Toy.Parser exposing (..)

import Char
import Parser exposing (..)
import Parser.LowLevel exposing (..)


type Module
    = Module (List (Pos Statement))


type alias Position =
    { row : Int
    , col : Int
    }


type alias Range =
    { start : Position
    , end : Position
    }


type alias Pos a =
    { range : Range
    , content : a
    }


type Statement
    = Assignment Identifier (Pos Expression)
    | TypeSignature Identifier (Pos TypeExp)


type alias Identifier =
    String


type TypeExp
    = ArrowType TypeExp (Maybe TypeExp)
    | AtomType String


type Expression
    = NumberLiteral String
    | StringLiteral String
    | Ref Identifier (List (Pos Expression))


module_ : Parser Module
module_ =
    inContext "module" <|
        succeed Module
            |= statements ""


statements : String -> Parser (List (Pos Statement))
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


statement : Parser (Pos Statement)
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
            |= positioned typeExp


typeExp : Parser TypeExp
typeExp =
    inContext "type expression" <|
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


expression : Parser (Pos Expression)
expression =
    inContext "expression" <|
        oneOf
            [ lazy (\_ -> ref)
            , positioned number
            ]


ref : Parser (Pos Expression)
ref =
    inContext "ref" <|
        positioned <|
            succeed Ref
                |= identifier
                |. spaces
                |= lazy (\_ -> functionTail)


functionTail : Parser (List (Pos Expression))
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


positioned : Parser a -> Parser (Pos a)
positioned parser =
    succeed
        (\( row1, col1 ) a ( row2, col2 ) ->
            Pos (Range (Position row1 col1) (Position row2 col2)) a
        )
        |= getPosition
        |= parser
        |= getPosition
