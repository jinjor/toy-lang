module Toy.Formatter exposing (formatInterface, formatType, formatRange, formatError)

import Toy.Position exposing (..)
import Toy.Type as T exposing (..)
import Toy.Parser as P exposing (..)
import Toy.Checker exposing (Interface)
import Toy.Error exposing (..)


formatInterface : Interface -> String
formatInterface v =
    formatInterfaceHelp v.id (Just v.type_)


formatInterfaceHelp : Identifier -> Maybe Type -> String
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


formatTypeExp : TypeExp -> String
formatTypeExp =
    formatTypeExpHelp False


formatTypeExpHelp : Bool -> TypeExp -> String
formatTypeExpHelp isArg type_ =
    case type_ of
        P.ArrowType head tail ->
            (formatTypeExpHelp False head ++ " -> " ++ formatTypeExpHelp False tail)
                |> parenIf isArg

        P.TypeValue constructor args ->
            (constructor :: List.map (formatTypeExpHelp True) args)
                |> String.join " "
                |> parenIf (isArg && args /= [])

        P.TypeVar name ->
            name


formatType : Type -> String
formatType t =
    -- TODO use the logic above
    case t of
        T.TypeVar id ->
            toString id

        T.TypeValue s args ->
            String.join " " (s :: List.map formatType args)

        T.TypeArrow t1 t2 ->
            "(" ++ formatType t1 ++ " -> " ++ formatType t2 ++ ")"

        T.TypeApply range t1 t2 ->
            "$(" ++ formatType t1 ++ ", " ++ formatType t2 ++ ")"


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
        DifinitionDuplicated name ->
            name ++ " is already defined"

        VariableNotDefined name ->
            name ++ " is not defined"

        TypeNotDefined name ->
            "type " ++ name ++ "is not defined"

        TypeMismatch expected actual ->
            "expected type "
                ++ formatType expected
                ++ " but got type "
                ++ formatType actual

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
                ++ formatType expected
                ++ " but implemantation is type "
                ++ formatType actual
