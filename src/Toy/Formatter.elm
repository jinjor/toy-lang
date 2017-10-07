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
formatTypeHelp isArg type_ =
    case type_ of
        ArrowType head tail ->
            formatTypeHelp False head
                ++ (case tail of
                        Just t ->
                            " -> " ++ formatTypeHelp False t

                        Nothing ->
                            ""
                   )
                |> parenIf isArg

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
