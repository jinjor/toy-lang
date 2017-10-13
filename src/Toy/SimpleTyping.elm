module Toy.SimpleTyping exposing (..)

import Dict exposing (Dict)


type Exp
    = IntLiteral
    | StringLiteral
    | Ref String
    | Lambda String Exp
    | Call Exp Exp


type alias Env =
    Dict String Type


type Type
    = TypeVar Int
    | TypeValue String
    | TypeRef String Int
    | TypeLambda String Int Type
    | TypeApply Type Type


type alias Context =
    { n : Int
    }


initialContext : Context
initialContext =
    Context 0


calc : Context -> Exp -> ( Type, Context )
calc context exp =
    case exp of
        IntLiteral ->
            ( TypeValue "Int", context )

        StringLiteral ->
            ( TypeValue "String", context )

        Ref a ->
            ( TypeRef a context.n
            , incr context
            )

        Lambda a exp ->
            let
                ( right, context1 ) =
                    calc (incr context) exp

                typeVarName =
                    context.n
            in
                ( TypeLambda a typeVarName right
                , context1
                )

        Call a b ->
            let
                ( first, context1 ) =
                    calc context a

                ( second, context2 ) =
                    calc context1 b
            in
                ( TypeApply first second
                , context2
                )


incr : Context -> Context
incr context =
    { context | n = context.n + 1 }


evaluate : Env -> Type -> Type
evaluate env t =
    case t of
        TypeRef a typeVarName ->
            env |> Dict.get a |> Maybe.withDefault t

        TypeLambda a typeVarName right ->
            TypeLambda a typeVarName (evaluate (Dict.insert a (TypeVar typeVarName) env) right)

        TypeApply first second ->
            apply (evaluate env first) (evaluate env second)

        _ ->
            t


apply : Type -> Type -> Type
apply first second =
    case first of
        TypeLambda a typeVarName right ->
            Debug.crash "not implemented"

        TypeValue _ ->
            Debug.crash "value cannot take arguments"

        _ ->
            TypeApply first second


{-|
  01: 1
  02: ""
  03: a
  04: f 1
  05: f 1 ""
  06: f a
  07: \a -> 1
  08: \a -> a
  09: \a -> increment a
  10: \a -> add a 1
  11: \a -> \b -> 1
  12: \a -> \b -> a
-}
examples =
    { e01 = test <| IntLiteral
    , e02 = test <| StringLiteral
    , e03 = test <| Ref "a"
    , e04 = test <| Call (Ref "f") IntLiteral
    , e05 = test <| Call (Call (Ref "f") IntLiteral) StringLiteral
    , e06 = test <| Call (Ref "f") (Ref "a")
    , e07 = test <| Lambda "a" IntLiteral
    , e08 = test <| Lambda "a" (Ref "a")
    , e09 = test <| Lambda "a" (Call (Ref "increment") IntLiteral)
    , e10 = test <| Lambda "a" (Call (Call (Ref "add") (Ref "a")) IntLiteral)
    , e11 = test <| Lambda "a" (Lambda "b" IntLiteral)
    , e12 = test <| Lambda "a" (Lambda "b" (Ref "a"))
    }


test =
    calc initialContext >> Tuple.first >> Debug.log "test-calc"


{-|
  01: a
  02: a
  03: \a -> b
  04: \a -> b
  05: a 1
  06: a 1
-}
examples2 =
    { e01 = test2 (TypeRef "a" 1) Dict.empty
    , e02 = test2 (TypeRef "a" 1) (Dict.fromList [ ( "a", TypeValue "Int" ) ])
    , e03 = test2 (TypeLambda "a" 1 (TypeRef "b" 2)) Dict.empty
    , e04 = test2 (TypeLambda "a" 1 (TypeRef "b" 2)) (Dict.fromList [ ( "b", TypeValue "Int" ) ])
    , e05 = test2 (TypeApply (TypeRef "a" 1) (TypeValue "Int")) Dict.empty
    , e06 = test2 (TypeApply (TypeRef "a" 1) (TypeValue "Int")) (Dict.fromList [ ( "a", TypeValue "Int" ) ])
    }


test2 t e =
    t
        |> evaluate e
        |> Debug.log "test-eval"
