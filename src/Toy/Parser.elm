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
            (positioned identifier
                |> andThen
                    (\id ->
                        succeed identity
                            |. spaces
                            |= oneOf
                                [ assignment id
                                , typeSignature id.content
                                ]
                    )
            )


typeSignature : Identifier -> Parser Statement
typeSignature id =
    inContext "type signature" <|
        succeed (TypeSignature id)
            |. symbol ":"
            |. spaces
            |= positioned typeExp


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


assignment : Pos Identifier -> Parser Statement
assignment id =
    inContext "assignment" <|
        succeed (Assignment id)
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
        succeed makeCall
            |= lazy (\_ -> singleExpression)
            |. spaces
            |= lazy (\_ -> functionTail)


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
                |= lazy (\_ -> expression)
                |. spaces
                |. symbol ")"
            ]


functionTail : Parser (List (Pos Expression))
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
            |= patterns
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


doReturn : Parser (Pos Expression)
doReturn =
    inContext "do return" <|
        succeed identity
            |. keyword "do"
            |. spaces
            |= oneOf
                [ lazy (\_ -> return)
                , lazy (\_ -> statement)
                    |> andThen
                        (\st ->
                            let
                                indent =
                                    String.repeat (st.range.start.col - 1) " "
                            in
                                succeed (\tail ret -> makeLet (st :: tail) ret)
                                    |= lazy (\_ -> statementsUntilReturn indent)
                                    |. spaces
                                    |= lazy (\_ -> return)
                        )
                ]


return : Parser (Pos Expression)
return =
    succeed identity
        |. keyword "return"
        |. spaces
        |= lazy (\_ -> expression)


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


statementsUntilReturn : String -> Parser (List (Pos Statement))
statementsUntilReturn indent =
    inContext "statements until return" <|
        succeed identity
            |= oneOf
                [ succeed []
                    |. keyword "return"
                , succeed (::)
                    |. symbol indent
                    |. spaces
                    |= lazy (\_ -> statement)
                    |= lazy (\_ -> statementsUntilReturn indent)
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
