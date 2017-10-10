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
    | JsRef JsIdentifier (List JsExpression)


translateModule : List C.Variable -> Module
translateModule interfaces =
    interfaces
        |> List.map (\v -> JsAssignment (fullId v.id) (translateExpression v.exp))
        |> Module


translateExpression : Maybe (P.Pos P.Expression) -> JsExpression
translateExpression maybeExp =
    case maybeExp of
        Nothing ->
            Debug.crash "bug in checker"

        Just exp ->
            case exp.content of
                P.NumberLiteral n ->
                    JsNumber n

                P.StringLiteral s ->
                    JsString s

                P.Ref id expressions ->
                    JsRef
                        (fullId id)
                        (List.map translateExpression <| List.map Just expressions)


fullId : String -> String
fullId id =
    "Main$" ++ id
