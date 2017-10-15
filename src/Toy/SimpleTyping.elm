module Toy.SimpleTyping exposing (..)

import Dict exposing (Dict)


type Exp
    = IntLiteral
    | StringLiteral
    | Ref String
    | Lambda String Exp
    | Call Exp Exp
    | Let String Exp Exp


type alias Env =
    Dict Int Type


type Type
    = TypeVar Int
    | TypeValue String
    | TypeArrow Type Type
    | TypeApply Type Type


calc : Int -> Dict String Int -> Dict String Type -> Exp -> ( Type, Int, Dict String Int )
calc n env typeVars exp =
    case exp of
        IntLiteral ->
            ( TypeValue "Int", n, env )

        StringLiteral ->
            ( TypeValue "String", n, env )

        Ref a ->
            typeVars
                |> Dict.get a
                |> Maybe.map (\t -> ( t, n, env ))
                |> Maybe.withDefault ( TypeVar n, n + 1, Dict.insert a n env )

        Lambda a exp ->
            let
                ( right, n1, env1 ) =
                    calc (n + 1) env (Dict.insert a (TypeVar n) typeVars) exp
            in
                ( TypeArrow (TypeVar n) right, n1, env1 )

        Call a b ->
            let
                ( first, n1, env1 ) =
                    calc n env typeVars a

                ( second, n2, env2 ) =
                    calc n1 env1 typeVars b
            in
                ( TypeApply first second, n2, env2 )

        Let name a b ->
            let
                ( first, n1, env1 ) =
                    calc n env (Dict.insert name (TypeVar n) typeVars) a
            in
                calc n1 env1 (Dict.insert name first typeVars) b


evaluate : Env -> Type -> Result String ( Type, Env )
evaluate env t =
    case t of
        TypeArrow arg right ->
            evaluate env right
                |> Result.map
                    (\( r, env ) ->
                        case arg of
                            TypeVar id ->
                                env
                                    |> Dict.get id
                                    |> Maybe.map
                                        (\t ->
                                            ( TypeArrow t r, env )
                                        )
                                    |> Maybe.withDefault ( TypeArrow arg r, env )

                            _ ->
                                ( TypeArrow arg r, env )
                    )

        TypeApply first second ->
            evaluate env first
                |> Result.andThen
                    (\( first, env ) ->
                        evaluate env second
                            |> Result.andThen
                                (\( second, env ) ->
                                    apply env first second
                                )
                    )

        TypeVar id ->
            Ok ( env |> Dict.get id |> Maybe.withDefault t, env )

        _ ->
            Ok ( t, env )


apply : Env -> Type -> Type -> Result String ( Type, Env )
apply env first second =
    case first of
        TypeArrow arg res ->
            case ( arg, second ) of
                ( TypeValue _, TypeVar id ) ->
                    Ok ( first, Dict.insert id arg env )

                ( TypeValue _, _ ) ->
                    if arg == second then
                        Ok ( res, env )
                    else
                        Err ("type mismatch: expected " ++ toString arg ++ " but got " ++ toString second)

                ( TypeVar id, _ ) ->
                    evaluate (Dict.insert id second env) res

                _ ->
                    apply env arg second

        TypeValue _ ->
            Err "value cannot take arguments"

        _ ->
            Ok ( TypeApply first second, env )


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
  13: let a = 1 in a
  14: let a = (\a -> a) in a
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
    , e13 = test <| Let "a" IntLiteral (Ref "a")
    , e14 = test <| Let "a" (Lambda "a" (Ref "a")) (Ref "a")
    }


test =
    calc 0 Dict.empty Dict.empty
        >> (\( t, _, env ) -> ( t, env ))
        >> Debug.log "test-calc"


{-|
  01: a -- env={}
  03: \a -> b -- env={}
  04: \a -> b -- env={ b: Int }
  05: \a -> a -- env={}
  09: (\a -> "") 1 -- env={}
  10: (\a -> a) 1 -- env={}
  12: (\a -> f a) -- env={ f: Int -> String }
  0v: a 1 -- env={ a: Int }
  0w: a 1 -- env={ a: a }
  0x: a 1 -- env={ a: Int -> String }
  0y: a 1 -- env={ a: String -> Int }
  0z: a 1 -- env={ a: a -> a }
-}
examples2 =
    { e01 = test2 (TypeVar 1) Dict.empty
    , e03 = test2 (TypeArrow (TypeVar 1) (TypeVar 2)) Dict.empty
    , e04 = test2 (TypeArrow (TypeVar 1) (TypeVar 2)) (Dict.singleton 2 (TypeValue "Int"))
    , e05 = test2 (TypeArrow (TypeVar 1) (TypeVar 1)) Dict.empty
    , e09 = test2 (TypeApply (TypeArrow (TypeVar 1) (TypeValue "String")) (TypeValue "Int")) Dict.empty
    , e10 = test2 (TypeApply (TypeArrow (TypeVar 1) (TypeVar 1)) (TypeValue "Int")) Dict.empty
    , e12 =
        test2
            (TypeArrow (TypeVar 1) (TypeApply (TypeVar 2) (TypeVar 1)))
            (Dict.singleton 2 (TypeArrow (TypeValue "Int") (TypeValue "String")))
    , e0v =
        test2
            (TypeApply (TypeVar 1) (TypeValue "Int"))
            (Dict.singleton 1 (TypeValue "Int"))
    , e0w =
        test2
            (TypeApply (TypeVar 1) (TypeValue "Int"))
            (Dict.singleton 1 (TypeVar 2))
    , e0x =
        test2
            (TypeApply (TypeVar 1) (TypeValue "Int"))
            (Dict.singleton 1 (TypeArrow (TypeValue "Int") (TypeValue "String")))
    , e0y =
        test2
            (TypeApply (TypeVar 1) (TypeValue "Int"))
            (Dict.singleton 1 (TypeArrow (TypeValue "String") (TypeValue "Int")))
    , e0z =
        test2
            (TypeApply (TypeVar 1) (TypeValue "Int"))
            (Dict.singleton 1 (TypeArrow (TypeVar 1) (TypeVar 1)))
    }


test2 t e =
    t
        |> evaluate e
        |> Result.map Tuple.first
        |> Debug.log "test-eval"
