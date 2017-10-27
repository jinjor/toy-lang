module Toy.Formatter exposing (formatInterface, formatRange, formatError)

import Toy.Position exposing (..)
import Toy.Parser exposing (..)
import Toy.Checker exposing (Interface)
import Toy.Error exposing (..)


formatInterface : Interface -> String
formatInterface v =
    formatInterfaceHelp v.id (Just v.type_)


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
            (formatTypeHelp False head ++ " -> " ++ formatTypeHelp False tail)
                |> parenIf isArg

        TypeValue constructor args ->
            (constructor :: List.map (formatTypeHelp True) args)
                |> String.join " "
                |> parenIf (isArg && args /= [])

        TypeVar name ->
            name


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

        TypeNotDefined name ->
            "type " ++ name ++ "is not defined"

        TypeMismatch expected actual ->
            "expected type "
                -- ++ formatType expected
                ++
                    toString expected
                ++ " but got type "
                -- ++ formatType actual
                ++
                    toString actual

        TooFewArguments ->
            "too few arguments"

        TooManyArguments ->
            "too many arguments"

        TooFewTypeArguments ->
            "too few type arguments"

        TooManyTypeArguments ->
            "too many type arguments"

        TypeSignatureMismatch expected actual ->
            "type is declared as "
                -- ++ formatType expected
                ++
                    toString expected
                ++ " but implemantation is type "
                -- ++ formatType actual
                ++
                    toString actual
