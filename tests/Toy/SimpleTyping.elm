module Toy.SimpleTyping exposing (..)

import Toy.SimpleParser as SimpleParser exposing (..)
import Dict exposing (Dict)


type alias Env =
    Dict Int Type


type Type
    = TypeVar Int
    | TypeValue String
    | TypeArrow Type Type
    | TypeApply Type Type


formatType : Type -> String
formatType t =
    case t of
        TypeVar id ->
            "$" ++ toString id

        TypeValue s ->
            s

        TypeArrow t1 t2 ->
            "(" ++ formatType t1 ++ " -> " ++ formatType t2 ++ ")"

        TypeApply t1 t2 ->
            "app(" ++ formatType t1 ++ ", " ++ formatType t2 ++ ")"


typeFromExp : TypeExp -> Type
typeFromExp t =
    case t of
        SimpleParser.ArrowType t1 t2 ->
            TypeArrow (typeFromExp t1) (typeFromExp t2)

        SimpleParser.TypeValue constructor _ ->
            TypeValue constructor

        SimpleParser.TypeVar name ->
            case name of
                "a" ->
                    TypeVar 1

                "b" ->
                    TypeVar 2

                "c" ->
                    TypeVar 3

                "d" ->
                    TypeVar 4

                "e" ->
                    TypeVar 5

                _ ->
                    Debug.crash "TODO"


calc : Int -> Dict String Int -> Dict String Type -> Expression -> ( Type, Int, Dict String Int )
calc n env typeVars exp =
    case exp of
        IntLiteral _ ->
            ( TypeValue "Int", n, env )

        StringLiteral _ ->
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
