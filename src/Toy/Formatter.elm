module Toy.Formatter exposing (formatInterface, formatError)

import Toy.Parser exposing (..)
import Toy.Checker exposing (Variable, Error, ErrorType(..))


formatInterface : Variable -> String
formatInterface v =
    formatInterfaceHelp v.id (Maybe.map Tuple.first v.type_)


formatInterfaceHelp : Identifier -> Maybe TypeExp -> String
formatInterfaceHelp id type_ =
    let
        typeString =
            case type_ of
                Just t ->
                    formatType t

                Nothing ->
                    "?"
    in
        id ++ " : " ++ typeString


formatType : TypeExp -> String
formatType =
    formatTypeHelp False


formatTypeHelp : Bool -> TypeExp -> String
formatTypeHelp isArg type_ =
    case type_ of
        ArrowType head tail ->
            case tail of
                Just t ->
                    (formatTypeHelp False head ++ " -> " ++ formatTypeHelp False t)
                        |> parenIf isArg

                Nothing ->
                    formatTypeHelp isArg head

        TypeValue constructor args ->
            (constructor :: List.map (formatTypeHelp True) args)
                |> String.join " "
                |> parenIf (isArg && args /= [])


parenIf : Bool -> String -> String
parenIf condition s =
    if condition then
        "(" ++ s ++ ")"
    else
        s


formatError : Error -> String
formatError ( range, e ) =
    formatRange range ++ " " ++ formatErrorType e


formatRange : Range -> String
formatRange range =
    formatPosition range.start ++ " " ++ formatPosition range.end


formatPosition : Position -> String
formatPosition pos =
    toString pos.row ++ ":" ++ toString pos.col


formatErrorType : ErrorType -> String
formatErrorType e =
    case e of
        VariableNotDefined id ->
            id ++ " is not defined"

        VariableDuplicated id ->
            id ++ " is already defined"

        TypeNotDefined name ->
            "type " ++ name ++ "is not defined"

        TypeDuplicated id ->
            id ++ " is already typed"

        TypeMismatch expected actual ->
            "expected type "
                ++ formatType expected
                ++ " but got type "
                ++ formatType actual

        TooManyArguments ->
            "too many arguments"

        NoImplementation id ->
            "no implementation found for " ++ id

        TypeSignatureMismatch expected actual ->
            "type is declared as "
                ++ formatType expected
                ++ " but implemantation is type "
                ++ formatType actual
