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
            toString id

        TypeValue s ->
            s

        TypeArrow t1 t2 ->
            "(" ++ formatType t1 ++ " -> " ++ formatType t2 ++ ")"

        TypeApply t1 t2 ->
            "$(" ++ formatType t1 ++ ", " ++ formatType t2 ++ ")"


fromTypeExp : TypeExp -> Type
fromTypeExp t =
    case t of
        SimpleParser.ArrowType t1 t2 ->
            TypeArrow (fromTypeExp t1) (fromTypeExp t2)

        SimpleParser.TypeValue constructor _ ->
            TypeValue constructor

        SimpleParser.TypeVar name ->
            case name of
                "a" ->
                    TypeVar 91

                "b" ->
                    TypeVar 92

                "c" ->
                    TypeVar 93

                "d" ->
                    TypeVar 94

                "e" ->
                    TypeVar 95

                _ ->
                    Debug.crash "TODO"


fromExp : Int -> Dict String Type -> Expression -> ( Type, Int, Dict String Int )
fromExp n typeVars exp =
    case exp of
        IntLiteral _ ->
            ( TypeValue "Int", n, Dict.empty )

        StringLiteral _ ->
            ( TypeValue "String", n, Dict.empty )

        Ref a ->
            typeVars
                |> Dict.get a
                |> Maybe.map (\t -> ( t, n, Dict.empty ))
                |> Maybe.withDefault ( TypeVar n, n + 1, Dict.singleton a n )

        Lambda a exp ->
            let
                ( right, n1, dep ) =
                    fromExp (n + 1) (Dict.insert a (TypeVar n) typeVars) exp
            in
                ( TypeArrow (TypeVar n) right, n1, dep )

        Call a b ->
            let
                ( first, n1, dep ) =
                    fromExp n typeVars a

                rightTypeVars =
                    Dict.union (Dict.map (\_ id -> TypeVar id) dep) typeVars

                ( second, n2, dep2 ) =
                    fromExp n1 rightTypeVars b
            in
                ( TypeApply first second, n2, Dict.union dep dep2 )

        Let name a b ->
            fromExp n typeVars (Call (Lambda name b) a)


debugEval : Env -> Type -> String
debugEval env t =
    Debug.log "eval" <|
        formatDict toString formatType env
            ++ " "
            ++ formatType t


assignEnv : Env -> Type -> Result String ( Type, Env )
assignEnv env t =
    case t of
        TypeArrow arg right ->
            assignEnv env right
                |> Result.map
                    (\( r, env ) ->
                        ( TypeArrow arg r, env )
                    )

        TypeApply first second ->
            assignEnv env first
                |> Result.andThen
                    (\( first, env ) ->
                        assignEnv env second
                            |> Result.map
                                (\( second, env ) ->
                                    ( TypeApply first second, env )
                                )
                    )

        TypeVar id ->
            Ok
                ( lookup env t
                , env
                )

        TypeValue _ ->
            Ok ( t, env )


lookup : Env -> Type -> Type
lookup env t =
    case t of
        TypeVar id ->
            Dict.get id env
                |> Maybe.map (lookup env)
                |> Maybe.withDefault t

        _ ->
            t


evaluate : Env -> Type -> Result String ( Type, Env )
evaluate env t =
    let
        _ =
            debugEval env t
    in
        case t of
            TypeArrow arg right ->
                evaluate env arg
                    |> Result.andThen
                        (\( arg, env ) ->
                            evaluate env right
                                |> Result.andThen
                                    (\( right, env ) ->
                                        assignEnv env arg
                                            |> Result.map
                                                (\( arg, env ) ->
                                                    ( TypeArrow arg right, env )
                                                )
                                    )
                        )

            TypeApply (TypeArrow arg right) second ->
                match env arg second
                    |> Result.andThen
                        (\env ->
                            evaluate env right
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

            _ ->
                assignEnv env t


apply : Env -> Type -> Type -> Result String ( Type, Env )
apply env first second =
    let
        _ =
            Debug.log "apply" ( first, second )
    in
        case first of
            TypeArrow arg res ->
                match env arg second
                    |> Result.andThen
                        (\env ->
                            assignEnv env res
                                |> Result.andThen
                                    (\( res, env ) ->
                                        evaluate env res
                                    )
                        )

            TypeValue name ->
                Err ("value " ++ name ++ " cannot take arguments")

            _ ->
                Ok ( TypeApply first second, env )


match : Env -> Type -> Type -> Result String Env
match env first second =
    case Debug.log "match" ( first, second ) of
        ( TypeValue a, TypeValue b ) ->
            if a == b then
                Ok env
            else
                Err ("type mismatch: expected " ++ a ++ " but got " ++ b)

        ( TypeValue a, TypeVar id ) ->
            Ok (Dict.insert id first env)

        ( TypeValue a, _ ) ->
            Err ("type mismatch: expected " ++ a ++ " but got " ++ formatType second)

        ( TypeVar id, _ ) ->
            Ok (Dict.insert id second env)

        -- a -> b, Int -> String
        --    => a == Int && b == String
        --    => Ok { a: Int, b: String }
        -- a -> a, Int -> Int
        --    => a == Int && a == Int == Int
        --    => Ok { a: Int }
        -- a -> a, Int -> String
        --    => a == Int && a == Int == String
        --    => Err
        -- (a -> b) -> (a -> b), Int -> String
        --    => (a -> b) == Int && (a -> b) == String
        --    => Err
        ( TypeArrow a1 a2, TypeArrow b1 b2 ) ->
            match env a1 b1
                |> Result.andThen
                    (\env ->
                        match env a2 b2
                    )

        -- a -> a, String
        --    => (a -> a) == String
        --    => Err
        ( TypeArrow a1 a2, _ ) ->
            Err "too few arguments"

        ( TypeApply _ _, _ ) ->
            Debug.crash "maybe a bug"


formatDict : (comparable -> String) -> (b -> String) -> Dict comparable b -> String
formatDict formatKey formatValue dict =
    "{ "
        ++ (dict
                |> Dict.toList
                |> List.map
                    (\( key, value ) ->
                        formatKey key ++ " => " ++ formatValue value
                    )
                |> String.join ", "
           )
        ++ " }"
