module Toy.Formatter exposing (formatInterface, formatType)

import Toy.Parser exposing (..)


formatInterface : Identifier -> Maybe TypeExp -> String
formatInterface id type_ =
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
formatTypeHelp paren type_ =
    case type_ of
        ArrowType head tail ->
            case tail of
                Just t ->
                    (formatTypeHelp False head ++ " -> " ++ formatTypeHelp False t)
                        |> parenIf paren

                Nothing ->
                    formatTypeHelp False head

        TypeValue constructor args ->
            (constructor :: List.map (formatTypeHelp True) args)
                |> String.join " "
                |> parenIf (paren && args /= [])


parenIf : Bool -> String -> String
parenIf condition s =
    if condition then
        "(" ++ s ++ ")"
    else
        s
