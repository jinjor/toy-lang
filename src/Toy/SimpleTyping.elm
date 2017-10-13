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
    | TypeArrow Type Type
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


evaluate : Env -> Type -> Result String Type
evaluate env t =
    case t of
        TypeRef a typeVarName ->
            Ok (env |> Dict.get a |> Maybe.withDefault t)

        TypeLambda a typeVarName right ->
            right
                |> evaluate
                    (Dict.union
                        (Dict.singleton a (TypeVar typeVarName))
                        env
                    )
                |> Result.map (TypeLambda a typeVarName)

        TypeApply first second ->
            Result.map2 (,) (evaluate env first) (evaluate env second)
                |> Result.andThen
                    (\( first, second ) ->
                        apply env first second
                    )

        TypeVar id ->
            Ok (env |> Dict.get (toString id) |> Maybe.withDefault t)

        _ ->
            Ok t


apply : Env -> Type -> Type -> Result String Type
apply env first second =
    case first of
        TypeLambda a typeVarName right ->
            evaluate
                (Dict.union
                    (Dict.singleton (toString typeVarName) second)
                    env
                )
                right

        TypeArrow arg res ->
            if arg == second then
                Ok res
            else
                Err ("type mismatch: expected " ++ toString arg ++ " but got " ++ toString second)

        TypeValue _ ->
            Err "value cannot take arguments"

        _ ->
            Ok (TypeApply first second)


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
  01: a -- env={}
  02: a -- env={ a: Int }
  03: \a -> b -- env={}
  04: \a -> b -- env={ b: Int }
  05: \a -> a -- env={}
  06: \a -> a -- env={ a: Int }
  07: a 1 -- env={}
  08: a 1 -- env={ a: Int }
  09: (\a -> "") 1 -- env={}
  10: (\a -> a) 1 -- env={}
  11: (\a -> a) 1 -- env={ a: String }
  12: (\a -> f a) -- env={ f: Int -> String }
  0x: a 1 -- env={ a: Int -> String }
  0y: a 1 -- env={ a: String -> Int }
-}
examples2 =
    { e01 = test2 (TypeRef "a" 1) Dict.empty
    , e02 = test2 (TypeRef "a" 1) (Dict.singleton "a" (TypeValue "Int"))
    , e03 = test2 (TypeLambda "a" 1 (TypeRef "b" 2)) Dict.empty
    , e04 = test2 (TypeLambda "a" 1 (TypeRef "b" 2)) (Dict.singleton "a" (TypeValue "Int"))
    , e05 = test2 (TypeLambda "a" 1 (TypeRef "a" 2)) Dict.empty
    , e06 = test2 (TypeLambda "a" 1 (TypeRef "a" 2)) (Dict.singleton "a" (TypeValue "Int"))
    , e07 = test2 (TypeApply (TypeRef "a" 1) (TypeValue "Int")) Dict.empty
    , e08 = test2 (TypeApply (TypeRef "a" 1) (TypeValue "Int")) (Dict.singleton "a" (TypeValue "Int"))
    , e09 = test2 (TypeApply (TypeLambda "a" 1 (TypeValue "String")) (TypeValue "Int")) Dict.empty
    , e10 = test2 (TypeApply (TypeLambda "a" 1 (TypeRef "a" 2)) (TypeValue "Int")) Dict.empty
    , e11 =
        test2
            (TypeApply (TypeLambda "a" 1 (TypeRef "a" 2)) (TypeValue "Int"))
            (Dict.singleton "a" (TypeValue "String"))
    , e12 =
        test2
            (TypeLambda "a" 1 (TypeApply (TypeRef "f" 2) (TypeRef "a" 3)))
            (Dict.singleton "f" (TypeArrow (TypeValue "Int") (TypeValue "String")))
    , e0x =
        test2
            (TypeApply (TypeRef "a" 1) (TypeValue "Int"))
            (Dict.singleton "a" (TypeArrow (TypeValue "Int") (TypeValue "String")))
    , e0y =
        test2
            (TypeApply (TypeRef "a" 1) (TypeValue "Int"))
            (Dict.singleton "a" (TypeArrow (TypeValue "String") (TypeValue "Int")))
    }


test2 t e =
    t
        |> evaluate e
        |> Debug.log "test-eval"
