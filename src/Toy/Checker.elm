module Toy.Checker exposing (..)

import Toy.Position exposing (..)
import Toy.Parser exposing (..)
import Toy.Typing as Typing exposing (..)
import Toy.Type as Type exposing (..)
import Toy.Error exposing (..)
import Dict exposing (Dict)
import Dict.Extra as DictX


type alias Interface =
    { id : String
    , type_ : Type
    }


type alias Implementation =
    { id : String
    , exp : Pos Expression
    }


type alias Defs =
    Dict String DefsForId


type alias DefsForId =
    { annotations : List Type
    , expressions : List ( Expression, Type, Dependency )
    }


check : Module -> ( List Error, List Interface, List Implementation )
check module_ =
    let
        defs =
            collectDefs module_.statements

        ( errors, dict, expDict ) =
            checkAllTypes def
    in
        ( errors
        , dict
            |> Dict.toList
            |> List.map (\( id, t ) -> Interface id t)
        , expDict
            |> Dict.toList
            |> List.map (\( id, exp ) -> Implementation id exp)
        )


checkAllTypes : Defs -> ( List Error, Dict String Type, Dict String Expression )
checkAllTypes defs =
    Dict.foldl checkForEachId ( [], Dict.empty, Dict.empty ) defs


checkForEachId :
    String
    -> DefsForId
    -> ( List Error, Dict String Type, Dict String Expression )
    -> ( List Error, Dict String Type, Dict String Expression )
checkForEachId id defsForId ( errors, envTypes, expDict ) =
    case List.map (validateType Type.knownTypes) defsForId.annotations of
        [] ->
            let
                ( inferenceErrors, inferedTypes ) =
                    evalExpressionsForId defs defsForId.expressions
            in
                case inferredTypes of
                    _ :: _ :: _ ->
                        ( Pos mockRange (DifinitionDuplicated id) :: inferenceErrors ++ errors
                        , envTypes
                        , expDict
                        )

                    ( exp, t ) :: [] ->
                        ( inferenceErrors ++ errors
                        , Dict.insert id t envTypes
                        , Dict.insert id exp expDict
                        )

                    [] ->
                        ( inferenceErrors ++ errors
                        , envTypes
                        , expDict
                        )

        [ t ] ->
            Debug.crash "skip for now"

        ts ->
            ( Pos mockRange (DifinitionDuplicated id) :: errors
            , envTypes
            , expDict
            )


evalExpressionsForId : Defs -> List ( Type, Dependency ) -> ( List Error, List ( Expression, Type ) )
evalExpressionsForId defs expressions =
    expressions
        |> List.map (\( exp, t, dep ) -> ( exp, evalOneExpression defs t dep ))
        |> List.foldl
            (\( exp, result ) ( inferenceErrors, inferedTypes ) ->
                case result of
                    Ok t ->
                        ( inferenceErrors, ( exp, t ) :: inferedTypes )

                    Err errs ->
                        ( errs ++ inferenceErrors, inferedTypes )
            )
            ( [], [] )


evalOneExpression : Defs -> Type -> Dependency -> Result (List Error) Type
evalOneExpression defs t dep =
    case resolveDependencies defs dep of
        ( True, [], env ) ->
            case evaluate env t of
                Ok ( t, env ) ->
                    Ok t

                Err e ->
                    Err [ e ]

        ( False, depErrors, _ ) ->
            Err depErrors


resolveDependencies : Defs -> Dependency -> ( Bool, List Error, Env )
resolveDependencies defs dep =
    dep
        |> Dict.toList
        |> List.foldl
            (\( name, ( range, id ) ) ( ok, errors, env ) ->
                case Dict.get name defs of
                    Just [] ->
                        Debug.log "impossible"

                    Just [ t ] ->
                        ( ok, errors, Dict.insert id t env )

                    Just ts ->
                        ( False, errors, env )

                    Nothing ->
                        ( False, ( range, VariableNotDefined name ) :: errors, env )
            )
            ( True, [], Dict.empty )


collectDefs : List (Pos Statement) -> Defs
collectDefs statements =
    statements
        |> DictX.groupBy idOf
        |> Dict.foldl
            (\id statements ( n, dict ) ->
                collectDefsForEachId n statements
                    |> Tuple.mapSecond
                        (\defsForId ->
                            Dict.insert id defsForId dict
                        )
            )
            ( 0, Dict.empty )
        |> Tuple.second


collectDefsForEachId : Int -> List (Pos Statement) -> ( Int, DefsForId )
collectDefsForEachId n statements =
    List.foldl updateDefsForIdByStatement ( n, DefsForId [] [] ) statements


updateDefsForIdByStatement : Statement -> ( Int, DefsForId ) -> ( Int, DefsForId )
updateDefsForIdByStatement statement ( n, defsForId ) =
    case statement.content of
        Assignment id exp ->
            let
                ( t, n1, dep ) =
                    Typing.fromExp n Dict.empty exp
            in
                ( n1
                , { defsForId
                    | expressions = defsForId.expressions ++ [ ( exp, t, dep ) ]
                  }
                )

        TypeSignature id exp ->
            let
                ( t, state ) =
                    Typing.fromTypeExp (Typing.initFromTypeExpState n) exp.content
            in
                ( state.n
                , { defsForId
                    | annotations = defsForId.annotations ++ [ t ]
                  }
                )


idOf : Pos Statement -> String
idOf statement =
    case statement.content of
        Assignment id _ ->
            id.content

        TypeSignature id _ ->
            id
