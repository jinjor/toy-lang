module Toy.Translator exposing (..)

import Toy.Position exposing (..)
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
    | JsFunction (List String) JsExpression


translateModule : List C.Implementation -> Module
translateModule impls =
    impls
        |> List.map
            (\impl ->
                JsAssignment (fullId impl.id) (translateExpression impl.exp)
            )
        |> Module


translateExpression : Pos P.Expression -> JsExpression
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

        P.Lambda patterns exp ->
            JsFunction (patternsToNames patterns) (translateExpression exp)

        P.Let _ _ _ ->
            Debug.crash "not implemented yet"


fullId : String -> String
fullId id =
    "Main$" ++ id


patternsToNames : P.Patterns -> List String
patternsToNames (P.Patterns pattern tail) =
    pattern
        :: (case tail of
                Just t ->
                    patternsToNames t

                Nothing ->
                    []
           )
