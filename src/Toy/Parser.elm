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
            |= statements 0


statements : Int -> Parser (List (Pos Statement))
statements indent =
    inContext "statements" <|
        succeed identity
            |. spacesWithLF
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
        delayedCommitMap (\id t -> TypeSignature id.content t)
            (succeed identity
                |= positioned identifier
                |. spacesWithLF
            )
            (succeed identity
                |. symbol ":"
                |. spacesWithLF
                |= positioned typeExp
            )


typeExp : Parser TypeExp
typeExp =
    inContext "type expression" <|
        (lazy (\_ -> singleTypeExp)
            |> andThen
                (\head ->
                    succeed identity
                        |. spacesWithLF
                        |= oneOf
                            [ succeed (ArrowType head)
                                |. symbol "->"
                                |. spacesWithLF
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
                |. spacesWithLF
                |= lazy (\_ -> typeExp)
                |. spacesWithLF
                |. symbol ")"
            , lazy (\_ -> typeValue)
            , typeVariable
            ]


typeValue : Parser TypeExp
typeValue =
    inContext "type value" <|
        succeed TypeValue
            |= typeConstructor
            |. spacesWithLF
            |= lazy (\_ -> typeArguments)


typeArguments : Parser (List TypeExp)
typeArguments =
    inContext "type arguments" <|
        oneOf
            [ succeed (::)
                |= lazy (\_ -> singleTypeExp)
                |. spacesWithLF
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


assignment : Int -> Parser Statement
assignment indent =
    inContext "assignment" <|
        delayedCommitMap Assignment
            (succeed identity
                |= positioned identifier
                |. spacesWithLF
            )
            (succeed identity
                |. symbol "="
                |. spacesWithLF
                |= lazy (\_ -> expression indent)
            )


identifier : Parser Identifier
identifier =
    inContext "identifier" <|
        source <|
            ignore (Exactly 1) Char.isLower
                |. ignore zeroOrMore (\c -> Char.isLower c || Char.isUpper c)


expression : Int -> Parser (Pos Expression)
expression indent =
    inContext "expression" <|
        succeed makeCall
            |= lazy (\_ -> singleExpression)
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


singleExpression : Parser (Pos Expression)
singleExpression =
    inContext "single expression" <|
        oneOf
            [ positioned number
            , positioned string
            , lazy (\_ -> positioned lambda)
            , lazy (\_ -> doReturn)
            , positioned ref
            , succeed identity
                |. symbol "("
                |. spacesWithLF
                |= lazy (\_ -> expression 0)
                |. spacesWithLF
                |. symbol ")"
            ]


functionTail : Int -> Parser (List (Pos Expression))
functionTail indent =
    inContext "function tail" <|
        oneOf
            [ (delayedCommit spacesWithLF
                (getCol
                    |> andThen
                        (\i ->
                            if i > indent then
                                succeed (::)
                                    |= lazy (\_ -> singleExpression)
                                    |= lazy (\_ -> functionTail indent)
                            else
                                succeed []
                        )
                )
              )
            , succeed []
            ]


lambda : Parser Expression
lambda =
    inContext "lambda" <|
        succeed Lambda
            |. symbol "\\"
            |= patterns
            |. spacesWithLF
            |. symbol "->"
            |. spacesWithLF
            |= lazy (\_ -> expression 0)


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


doReturn : Parser (Pos Expression)
doReturn =
    inContext "do return" <|
        succeed makeLet
            |. keyword "do"
            |. spaces
            |= (getCol
                    |> andThen
                        (\i ->
                            oneOf
                                [ delayedCommitMap (::)
                                    (lazy (\_ -> statement i))
                                    (lazy (\_ -> statementsUntilReturn i))
                                , succeed []
                                ]
                        )
               )
            |. keyword "return"
            |. spaces
            |= lazy (\_ -> expression 0)


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


statementsUntilReturn : Int -> Parser (List (Pos Statement))
statementsUntilReturn indent =
    inContext "statements until return" <|
        succeed identity
            |= oneOf
                [ succeed []
                    |. keyword "return"
                , succeed (::)
                    |. spaces
                    |. checkIndentLevel indent
                    |= lazy (\_ -> statement indent)
                    |= lazy (\_ -> statementsUntilReturn indent)
                ]


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


number : Parser Expression
number =
    inContext "number" <|
        succeed IntLiteral
            |= oneOf
                [ map toString int
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


spacesWithLF : Parser ()
spacesWithLF =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


spaces1 : Parser ()
spaces1 =
    ignore oneOrMore (\c -> c == ' ')


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


keywords : Set String
keywords =
    Set.fromList
        [ "do"
        , "return"
        ]
