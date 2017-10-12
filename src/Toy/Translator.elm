module Toy.Translator exposing (..)

import Toy.Parser as P
import Toy.Checker as C
import Dict exposing (Dict)


type Module
    = Module (List JsStatement)


type JsStatement
    = JsAssignment JsIdentifier JsExpression


type alias JsIdentifier =
    String


type JsExpression
    = JsNumber String
    | JsString String
    | JsRef JsIdentifier
    | JsCall JsExpression JsExpression


translateModule : List C.Variable -> Module
translateModule interfaces =
    interfaces
        |> List.map (\v -> JsAssignment (fullId v.id) (translateExpressionWrap v.exp))
        |> Module


translateExpressionWrap : Maybe (P.Pos P.Expression) -> JsExpression
translateExpressionWrap maybeExp =
    case maybeExp of
        Nothing ->
            Debug.crash "bug in checker"

        Just exp ->
            translateExpression exp


translateExpression : P.Pos P.Expression -> JsExpression
translateExpression exp =
    case exp.content of
        P.NumberLiteral n ->
            JsNumber n

        P.StringLiteral s ->
            JsString s

        P.Ref id ->
            JsRef (fullId id)

        P.Call first second ->
            JsCall (translateExpression first) (translateExpression second)

        P.Lambda _ _ ->
            Debug.crash "not implemented yet!"


fullId : String -> String
fullId id =
    "Main$" ++ id
