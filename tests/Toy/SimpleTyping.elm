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


fromTypeExp : Int -> Dict String Type -> TypeExp -> ( Type, Int, Dict String Type )
fromTypeExp n typeVars t =
    case t of
        SimpleParser.ArrowType t1 t2 ->
            fromTypeExp n typeVars t1
                |> (\( t1, n, typeVars ) ->
                        fromTypeExp n typeVars t2
                            |> (\( t2, n, typeVars ) ->
                                    ( TypeArrow t1 t2, n, typeVars )
                               )
                   )

        SimpleParser.TypeValue constructor _ ->
            ( TypeValue constructor, n, typeVars )

        SimpleParser.TypeVar name ->
            typeVars
                |> Dict.get name
                |> Maybe.map (\t -> ( t, n, typeVars ))
                |> Maybe.withDefault
                    ( TypeVar n
                    , n + 1
                    , Dict.insert name (TypeVar n) typeVars
                    )


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


debugEval : Env -> Type -> Type
debugEval env t =
    let
        _ =
            Debug.log "eval" <|
                formatDict toString formatType env
                    ++ " "
                    ++ formatType t
    in
        t


assignEnv : Env -> Type -> Type
assignEnv env t =
    case t of
        TypeArrow arg right ->
            TypeArrow (assignEnv env arg) (assignEnv env right)

        TypeApply first second ->
            TypeApply (assignEnv env first) (assignEnv env second)

        TypeVar id ->
            lookup env t

        TypeValue _ ->
            t


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
    case debugEval env t of
        TypeArrow arg right ->
            evaluate env arg
                |> Result.andThen
                    (\( arg, env ) ->
                        evaluate env right
                            |> Result.map
                                (\( right, env ) ->
                                    ( TypeArrow (assignEnv env arg) right
                                    , env
                                    )
                                )
                    )

        TypeApply first second ->
            apply env first second

        _ ->
            Ok ( assignEnv env t, env )


apply : Env -> Type -> Type -> Result String ( Type, Env )
apply env first second =
    case first of
        TypeArrow arg right ->
            evaluate env second
                |> Result.andThen
                    (\( second, env ) ->
                        match env arg second
                            |> Result.andThen
                                (\env ->
                                    evaluate env right
                                )
                    )

        TypeValue name ->
            Err ("value " ++ name ++ " cannot take arguments")

        _ ->
            evaluate env first
                |> Result.andThen
                    (\( first, env ) ->
                        case first of
                            TypeArrow _ _ ->
                                apply env first second

                            TypeValue _ ->
                                apply env first second

                            _ ->
                                Ok ( TypeApply first second, env )
                    )


match : Env -> Type -> Type -> Result String Env
match env first second =
    case ( first, second ) of
        ( TypeValue a, TypeValue b ) ->
            if a == b then
                matchTypeArgs env [] []
            else
                Err ("type mismatch: expected " ++ a ++ " but got " ++ b)

        ( TypeValue a, TypeVar id ) ->
            Ok (Dict.insert id first env)

        ( TypeValue a, _ ) ->
            Err ("type mismatch: expected " ++ a ++ " but got " ++ formatType second)

        ( TypeVar id, _ ) ->
            Ok (Dict.insert id second env)

        ( TypeArrow a1 a2, TypeArrow b1 b2 ) ->
            match env a1 b1
                |> Result.andThen
                    (\env ->
                        match env a2 b2
                    )

        ( TypeArrow a1 a2, _ ) ->
            Err "too few arguments"

        ( TypeApply _ _, _ ) ->
            Debug.crash "maybe a bug"


matchTypeArgs : Env -> List Type -> List Type -> Result String Env
matchTypeArgs env first second =
    case ( first, second ) of
        ( [], [] ) ->
            Ok env

        ( x :: xs, y :: ys ) ->
            match env x y
                |> Result.andThen
                    (\env ->
                        matchTypeArgs env xs ys
                    )

        ( [], _ ) ->
            Err "too many type arguments"

        ( _, [] ) ->
            Err "too few type arguments"


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
