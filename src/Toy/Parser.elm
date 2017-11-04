module Toy.Parser exposing (..)

import Char
import Set exposing (Set)
import Parser exposing (..)
import Parser.LowLevel exposing (..)
import Toy.Position exposing (..)


type alias Module =
    { name : String
    , statements : List (Pos Statement)
    }


type Statement
    = Assignment (Pos Identifier) (Pos Expression)
    | TypeSignature Identifier (Pos TypeExp)


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
    | ListLiteral (List (Pos Expression))
    | Ref Identifier
    | Call (Pos Expression) (Pos Expression)
    | Lambda Patterns (Pos Expression)
    | Let Identifier (Pos Expression) (Pos Expression)


type Patterns
    = Patterns Pattern (Maybe Patterns)


type alias Pattern =
    String


mockRange : Range
mockRange =
    Range (Position 0 0) (Position 0 0)


module_ : Parser Module
module_ =
    inContext "module" <|
        succeed (Module "Main")
            |= statements 1
            |. spaces
            |. end


statements : Int -> Parser (List (Pos Statement))
statements indent =
    inContext "statements" <|
        succeed identity
            |. spaces
            |= oneOf
                [ succeed []
                    |. end
                , getCol
                    |> andThen
                        (\i ->
                            if i == indent then
                                succeed (::)
                                    |= statement indent
                                    |= statements indent
                            else if i > indent then
                                fail "indent error 2"
                            else
                                succeed []
                        )
                ]


emptyLine : Parser ()
emptyLine =
    ignore zeroOrMore (\c -> c == ' ')
        |. symbol "\n"


statement : Int -> Parser (Pos Statement)
statement indent =
    inContext "statement" <|
        positioned <|
            oneOf
                [ typeSignature
                , lazy (\_ -> assignment indent)
                ]


typeSignature : Parser Statement
typeSignature =
    inContext "type signature" <|
        (getCol
            |> andThen
                (\indent ->
                    delayedCommitMap (\id t -> TypeSignature id.content t)
                        (succeed identity
                            |= positioned identifier
                            |. spaces
                            |. symbol ":"
                        )
                        (succeed identity
                            |. spaces
                            |= positioned (typeExp indent)
                        )
                )
        )


typeExp : Int -> Parser TypeExp
typeExp indent =
    inContext "type expression" <|
        (lazy (\_ -> singleTypeExp indent)
            |> andThen
                (\head ->
                    succeed identity
                        |. spaces
                        |= oneOf
                            [ succeed (ArrowType head)
                                |. symbol "->"
                                |. spaces
                                |= lazy (\_ -> typeExp indent)
                            , succeed head
                            ]
                )
        )


singleTypeExp : Int -> Parser TypeExp
singleTypeExp indent =
    inContext "single type expression" <|
        oneOf
            [ succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> typeExp indent)
                |. spaces
                |. symbol ")"
            , lazy (\_ -> typeValue indent)
            , typeVariable
            ]


typeValue : Int -> Parser TypeExp
typeValue indent =
    inContext "type value" <|
        succeed TypeValue
            |= typeConstructor
            |. spaces
            |= lazy (\_ -> typeArguments indent)


typeArguments : Int -> Parser (List TypeExp)
typeArguments indent =
    inContext "type arguments" <|
        oneOf
            [ succeed []
                |. end
            , getCol
                |> andThen
                    (\i ->
                        if i > indent then
                            oneOf
                                [ succeed (::)
                                    |= lazy (\_ -> singleTypeExp indent)
                                    |. spaces
                                    |= lazy (\_ -> typeArguments indent)
                                , succeed []
                                ]
                        else
                            succeed []
                    )
            ]


typeConstructor : Parser String
typeConstructor =
    inContext "type constructor" upperCamel


typeVariable : Parser TypeExp
typeVariable =
    inContext "type variable" <|
        map TypeVar lowerCamel


assignment : Int -> Parser Statement
assignment indent =
    inContext "assignment" <|
        delayedCommitMap Assignment
            (succeed identity
                |= positioned identifier
                |. spaces
            )
            (succeed identity
                |. symbol "="
                |. spaces
                |= lazy (\_ -> expression indent)
            )


identifier : Parser Identifier
identifier =
    inContext "identifier" <|
        (lowerCamel
            |> andThen
                (\name ->
                    if name == "return" then
                        fail "unexpected keyword `return`"
                    else
                        succeed name
                )
        )


expression : Int -> Parser (Pos Expression)
expression indent =
    inContext "expression" <|
        succeed makeCall
            |= lazy (\_ -> singleExpression indent)
            |= lazy (\_ -> functionTail indent)


makeCall : Pos Expression -> List (Pos Expression) -> Pos Expression
makeCall head tail =
    case tail of
        [] ->
            head

        x :: xs ->
            let
                range =
                    Range head.range.start x.range.end
            in
                makeCall (Pos range (Call head x)) xs


singleExpression : Int -> Parser (Pos Expression)
singleExpression indent =
    inContext "single expression" <|
        oneOf
            [ positioned intLiteral
            , positioned stringLiteral
            , positioned (listLiteral indent)
            , lazy (\_ -> positioned (lambda indent))
            , lazy (\_ -> doReturn)
            , positioned ref
            , succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> expression 0)
                |. spaces
                |. symbol ")"
            ]


functionTail : Int -> Parser (List (Pos Expression))
functionTail indent =
    inContext "function tail" <|
        delayedCommit spaces
            (getCol
                |> andThen
                    (\i ->
                        if i > indent then
                            oneOf
                                [ delayedCommitMap (::)
                                    (lazy (\_ -> singleExpression indent))
                                    (lazy (\_ -> functionTail indent))
                                , succeed []
                                ]
                        else
                            succeed []
                    )
            )


lambda : Int -> Parser Expression
lambda indent =
    inContext "lambda" <|
        succeed Lambda
            |. symbol "\\"
            |= patterns
            |. spaces
            |. symbol "->"
            |. spaces
            |= lazy (\_ -> expression indent)


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
    inContext "pattern" lowerCamel


ref : Parser Expression
ref =
    inContext "ref" <|
        succeed Ref
            |= identifier


doReturn : Parser (Pos Expression)
doReturn =
    inContext "do return" <|
        delayedCommit
            (succeed ()
                |. keyword "do"
                |. spaces1
            )
            (getCol
                |> andThen
                    (\i ->
                        doReturnTail i []
                    )
            )


doReturnTail : Int -> List (Pos Statement) -> Parser (Pos Expression)
doReturnTail indent statements =
    oneOf
        [ delayedCommit
            (succeed ()
                |. keyword "return"
                |. spaces1
            )
            (succeed (makeLet statements)
                |= lazy (\_ -> expression indent)
            )
        , succeed identity
            |. checkIndentLevel indent
            |= (lazy (\_ -> statement indent)
                    |> andThen
                        (\s ->
                            succeed identity
                                |. spaces
                                |= doReturnTail indent (statements ++ [ s ])
                        )
               )
        ]


makeLet : List (Pos Statement) -> Pos Expression -> Pos Expression
makeLet statements exp =
    case statements of
        [] ->
            exp

        x :: xs ->
            case x.content of
                Assignment left right ->
                    Pos mockRange (Let left.content right (makeLet xs exp))

                _ ->
                    Debug.crash "not implemented yet"


checkIndentLevel : Int -> Parser ()
checkIndentLevel expected =
    getCol
        |> andThen
            (\i ->
                if i == expected then
                    succeed ()
                else
                    fail "indent error"
            )


intLiteral : Parser Expression
intLiteral =
    inContext "int literal" <|
        succeed IntLiteral
            |= map toString int


stringLiteral : Parser Expression
stringLiteral =
    inContext "string literal" <|
        succeed StringLiteral
            |. symbol "\""
            |= (source <|
                    ignore zeroOrMore (\c -> c /= '"')
               )
            |. symbol "\""


listLiteral : Int -> Parser Expression
listLiteral indent =
    inContext "list literal" <|
        succeed ListLiteral
            |. symbol "["
            |. spaces
            |= listLiteralTail indent


listLiteralTail : Int -> Parser (List (Pos Expression))
listLiteralTail indent =
    oneOf
        [ succeed []
            |. symbol "]"
        , succeed (::)
            |= expression indent
            |. spaces
            |= lazy (\_ -> listLiteralTail indent)
        ]


lowerCamel : Parser String
lowerCamel =
    camel False


upperCamel : Parser String
upperCamel =
    camel True


camel : Bool -> Parser String
camel upper =
    source <|
        ignore (Exactly 1)
            (if upper then
                Char.isUpper
             else
                Char.isLower
            )
            |. ignore zeroOrMore (\c -> Char.isLower c || Char.isUpper c)


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


spaces1 : Parser ()
spaces1 =
    ignore oneOrMore (\c -> c == ' ' || c == '\n')


positioned : Parser a -> Parser (Pos a)
positioned parser =
    succeed
        (\start a end ->
            Pos (Range start end) a
        )
        |= getPos
        |= parser
        |= getPos


getPos : Parser Position
getPos =
    map (\( row, col ) -> Position row col) getPosition


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
            "internal error (bad repeat)"

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


keywords : Set String
keywords =
    Set.fromList
        [ "do"
        , "return"
        ]
