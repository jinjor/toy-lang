module Toy.SimpleTyping exposing (..)

import Toy.Parser as P exposing (Range, Position)
import Toy.SimpleParser as SimpleParser exposing (..)
import Dict exposing (Dict)


type alias Env =
    Dict Int Type


type Type
    = TypeVar Int
    | TypeValue String (List Type)
    | TypeArrow Type Type
    | TypeApply Range Type Type


type Error
    = Error Range String


mockRange : Range
mockRange =
    Range (Position 0 0) (Position 0 0)


formatType : Type -> String
formatType t =
    case t of
        TypeVar id ->
            toString id

        TypeValue s args ->
            String.join " " (s :: List.map formatType args)

        TypeArrow t1 t2 ->
            "(" ++ formatType t1 ++ " -> " ++ formatType t2 ++ ")"

        TypeApply _ t1 t2 ->
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

        SimpleParser.TypeValue constructor args ->
            args
                |> List.foldl
                    (\arg ( argTypes, n, typeVars ) ->
                        fromTypeExp n typeVars arg
                            |> (\( argType, n, typeVars ) ->
                                    ( argTypes ++ [ argType ], n, typeVars )
                               )
                    )
                    ( [], n, typeVars )
                |> (\( argTypes, n, typeVars ) ->
                        ( TypeValue constructor argTypes, n, typeVars )
                   )

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
            ( TypeValue "Int" [], n, Dict.empty )

        StringLiteral _ ->
            ( TypeValue "String" [], n, Dict.empty )

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
                ( TypeApply mockRange first second, n2, Dict.union dep dep2 )

        Let name a b ->
            fromExp n typeVars (Call (Lambda name b) a)


fromOriginalExp : Int -> Dict String Type -> P.Expression -> ( Type, Int, Dict String Int )
fromOriginalExp n typeVars exp =
    case exp of
        P.NumberLiteral _ ->
            ( TypeValue "Int" [], n, Dict.empty )

        P.StringLiteral _ ->
            ( TypeValue "String" [], n, Dict.empty )

        P.Ref a ->
            typeVars
                |> Dict.get a
                |> Maybe.map (\t -> ( t, n, Dict.empty ))
                |> Maybe.withDefault ( TypeVar n, n + 1, Dict.singleton a n )

        P.Lambda (P.Patterns a _) exp ->
            let
                ( right, n1, dep ) =
                    fromOriginalExp (n + 1) (Dict.insert a (TypeVar n) typeVars) exp.content
            in
                ( TypeArrow (TypeVar n) right, n1, dep )

        P.Call a b ->
            let
                ( first, n1, dep ) =
                    fromOriginalExp n typeVars a.content

                rightTypeVars =
                    Dict.union (Dict.map (\_ id -> TypeVar id) dep) typeVars

                ( second, n2, dep2 ) =
                    fromOriginalExp n1 rightTypeVars b.content
            in
                ( TypeApply a.range first second, n2, Dict.union dep dep2 )

        P.Let name a b ->
            fromOriginalExp
                n
                typeVars
                (P.Call (P.Pos mockRange (P.Lambda (P.Patterns name Nothing) (P.Pos mockRange b))) (P.Pos mockRange a))


debugEval : Env -> Type -> Type
debugEval env t =
    -- let
    --     _ =
    --         Debug.log "eval" <|
    --             formatDict toString formatType env
    --                 ++ " "
    --                 ++ formatType t
    -- in
    t


assignEnv : Env -> Type -> Type
assignEnv env t =
    case t of
        TypeArrow arg right ->
            TypeArrow (assignEnv env arg) (assignEnv env right)

        TypeApply range first second ->
            TypeApply range (assignEnv env first) (assignEnv env second)

        TypeVar id ->
            lookup env t

        TypeValue constructor args ->
            TypeValue constructor (List.map (assignEnv env) args)


lookup : Env -> Type -> Type
lookup env t =
    case t of
        TypeVar id ->
            Dict.get id env
                |> Maybe.map (lookup env)
                |> Maybe.withDefault t

        _ ->
            t


evaluate : Env -> Type -> Result Error ( Type, Env )
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

        TypeApply range first second ->
            apply env range first second

        _ ->
            Ok ( assignEnv env t, env )


apply : Env -> Range -> Type -> Type -> Result Error ( Type, Env )
apply env range first second =
    case first of
        TypeArrow arg right ->
            evaluate env second
                |> Result.andThen
                    (\( second, env ) ->
                        match env arg second
                            |> Result.mapError (\s -> Error range s)
                            |> Result.andThen
                                (\env ->
                                    evaluate env right
                                )
                    )

        TypeValue name _ ->
            Err (Error mockRange <| "value " ++ name ++ " cannot take arguments")

        _ ->
            evaluate env first
                |> Result.andThen
                    (\( first, env ) ->
                        case first of
                            TypeArrow _ _ ->
                                apply env range first second

                            TypeValue _ _ ->
                                apply env range first second

                            _ ->
                                Ok ( TypeApply range first second, env )
                    )


match : Env -> Type -> Type -> Result String Env
match env first second =
    case ( first, second ) of
        ( TypeValue c1 args1, TypeValue c2 args2 ) ->
            if c1 == c2 then
                matchTypeArgs env args1 args2
            else
                Err ("type mismatch: expected " ++ formatType first ++ " but got " ++ formatType second)

        ( TypeValue a _, TypeVar id ) ->
            Ok (Dict.insert id first env)

        ( TypeValue a _, _ ) ->
            Err ("type mismatch: expected " ++ formatType first ++ " but got " ++ formatType second)

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

        ( TypeApply _ _ _, _ ) ->
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
