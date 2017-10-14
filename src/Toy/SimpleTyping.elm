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
    | TypeRef String
    | TypeLambda Int Type
    | TypeArrow Type Type
    | TypeApply Type Type


calc : Int -> Dict String Int -> Exp -> ( Type, Int )
calc n typeVars exp =
    case exp of
        IntLiteral ->
            ( TypeValue "Int", n )

        StringLiteral ->
            ( TypeValue "String", n )

        Ref a ->
            typeVars
                |> Dict.get a
                |> Maybe.map (\id -> TypeVar id)
                |> Maybe.withDefault (TypeRef a)
                |> (\t -> ( t, n ))

        Lambda a exp ->
            let
                ( right, n1 ) =
                    calc (n + 1) (Dict.insert a (n + 1) typeVars) exp
            in
                ( TypeLambda n right
                , n1
                )

        Call a b ->
            let
                ( first, n1 ) =
                    calc n typeVars a

                ( second, n2 ) =
                    calc n1 typeVars b
            in
                ( TypeApply first second
                , n2
                )


evaluate : Env -> Type -> Result String Type
evaluate env t =
    case t of
        TypeRef a ->
            Ok (env |> Dict.get a |> Maybe.withDefault t)

        TypeLambda id right ->
            evaluate env right
                |> Result.map (TypeLambda id)

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
        TypeLambda id right ->
            evaluate (Dict.insert (toString id) second env) right

        TypeArrow arg res ->
            case ( arg, second ) of
                ( _, TypeVar id ) ->
                    Err ("TODO1: resolved " ++ toString id ++ " = " ++ toString arg)

                ( TypeVar id, _ ) ->
                    Err "TODO2: arg cannot be tyep variable"

                _ ->
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
    calc 0 Dict.empty >> Tuple.first >> Debug.log "test-calc"


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
  *12: (\a -> f a) -- env={ f: Int -> String }
  0x: a 1 -- env={ a: Int -> String }
  0y: a 1 -- env={ a: String -> Int }
  0z: a 1 -- env={ a: a -> a }
-}
examples2 =
    { e01 = test2 (TypeRef "a") Dict.empty
    , e02 = test2 (TypeRef "a") (Dict.singleton "a" (TypeValue "Int"))
    , e03 = test2 (TypeLambda 1 (TypeRef "b")) Dict.empty
    , e04 = test2 (TypeLambda 1 (TypeRef "b")) (Dict.singleton "b" (TypeValue "Int"))
    , e05 = test2 (TypeLambda 1 (TypeVar 1)) Dict.empty
    , e06 = test2 (TypeLambda 1 (TypeVar 1)) (Dict.singleton "a" (TypeValue "Int"))
    , e07 = test2 (TypeApply (TypeRef "a") (TypeValue "Int")) Dict.empty
    , e08 = test2 (TypeApply (TypeRef "a") (TypeValue "Int")) (Dict.singleton "a" (TypeValue "Int"))
    , e09 = test2 (TypeApply (TypeLambda 1 (TypeValue "String")) (TypeValue "Int")) Dict.empty
    , e10 = test2 (TypeApply (TypeLambda 1 (TypeVar 1)) (TypeValue "Int")) Dict.empty
    , e11 =
        test2
            (TypeApply (TypeLambda 1 (TypeVar 1)) (TypeValue "Int"))
            (Dict.singleton "a" (TypeValue "String"))
    , e12 =
        test2
            (TypeLambda 1 (TypeApply (TypeRef "f") (TypeVar 1)))
            (Dict.singleton "f" (TypeArrow (TypeValue "Int") (TypeValue "String")))
    , e0x =
        test2
            (TypeApply (TypeRef "a") (TypeValue "Int"))
            (Dict.singleton "a" (TypeArrow (TypeValue "Int") (TypeValue "String")))
    , e0y =
        test2
            (TypeApply (TypeRef "a") (TypeValue "Int"))
            (Dict.singleton "a" (TypeArrow (TypeValue "String") (TypeValue "Int")))
    , e0z =
        test2
            (TypeApply (TypeRef "a") (TypeValue "Int"))
            (Dict.singleton "a" (TypeLambda 1 (TypeVar 1)))
    }


test2 t e =
    t
        |> evaluate e
        |> Debug.log "test-eval"
