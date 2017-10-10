module Toy.Generator exposing (..)

import Toy.Translator exposing (..)


generateModule : Module -> String
generateModule (Module statements) =
    statements
        |> List.map generateStatement
        |> String.join "\n"


generateStatement : JsStatement -> String
generateStatement (JsAssignment id exp) =
    "const " ++ generateIdentifier id ++ " = " ++ generateExpression exp ++ ";\n"


generateExpression : JsExpression -> String
generateExpression exp =
    case exp of
        JsNumber num ->
            num

        JsString str ->
            "\"" ++ str ++ "\""

        JsRef id args ->
            generateIdentifier id ++ String.join "" (List.map generateArg args)


generateIdentifier : JsIdentifier -> String
generateIdentifier id =
    id


generateArg : JsExpression -> String
generateArg arg =
    "(" ++ generateExpression arg ++ ")"
