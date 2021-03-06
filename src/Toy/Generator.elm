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

        JsRef id ->
            generateIdentifier id

        JsCall first second ->
            generateExpression first ++ "(" ++ generateExpression second ++ ")"

        JsFunction args exp ->
            "function (" ++ String.join "," args ++ ") { return " ++ generateExpression exp ++ "; }"


generateIdentifier : JsIdentifier -> String
generateIdentifier id =
    id
