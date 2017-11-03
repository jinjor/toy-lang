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
                |. spaces
            )
            (succeed identity
                |. symbol ":"
                |. spaces
                |= positioned typeExp
            )


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


lowerCamel : Parser String
lowerCamel =
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
                                    (lazy (\_ -> singleExpression))
                                    (lazy (\_ -> functionTail indent))
                                , succeed []
                                ]
                        else
                            succeed []
                    )
            )


lambda : Parser Expression
lambda =
    inContext "lambda" <|
        succeed Lambda
            |. symbol "\\"
            |= patterns
            |. spaces
            |. symbol "->"
            |. spaces
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
