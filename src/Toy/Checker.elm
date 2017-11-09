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
    , expressions : List ( Pos Expression, Type, Dependency )
    }


check : Module -> ( List Error, List Interface, List Implementation )
check module_ =
    let
        defs =
            collectDefs module_.statements

        ( errors, dict, expDict ) =
            checkAllTypes defs
    in
        ( errors
        , dict
            |> Dict.toList
            |> List.map (\( id, t ) -> Interface id t)
        , expDict
            |> Dict.toList
            |> List.map (\( id, exp ) -> Implementation id exp)
        )


checkAllTypes : Defs -> ( List Error, Dict String Type, Dict String (Pos Expression) )
checkAllTypes defs =
    Dict.foldl checkForEachId ( [], Dict.empty, Dict.empty ) defs


checkForEachId :
    String
    -> DefsForId
    -> ( List Error, Dict String Type, Dict String (Pos Expression) )
    -> ( List Error, Dict String Type, Dict String (Pos Expression) )
checkForEachId id defsForId ( errors, envTypes, expDict ) =
    case defsForId.annotations of
        [] ->
            let
                ( inferenceErrors, inferedTypes ) =
                    evalExpressionsForId envTypes defsForId.expressions
            in
                case inferedTypes of
                    _ :: _ :: _ ->
                        ( ( mockRange, DifinitionDuplicated id ) :: inferenceErrors ++ errors
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

        [ annotation ] ->
            let
                annotationErrors =
                    validateType Type.knownTypes annotation
            in
                if annotationErrors == [] then
                    let
                        newEnvTypes =
                            Dict.insert id annotation envTypes

                        ( inferenceErrors, inferedTypes ) =
                            evalExpressionsForId newEnvTypes defsForId.expressions
                    in
                        case inferedTypes of
                            _ :: _ :: _ ->
                                ( ( mockRange, DifinitionDuplicated id ) :: inferenceErrors ++ errors
                                , newEnvTypes
                                , expDict
                                )

                            ( exp, t ) :: [] ->
                                case match Dict.empty t annotation of
                                    Ok _ ->
                                        ( inferenceErrors ++ errors
                                        , newEnvTypes
                                        , Dict.insert id exp expDict
                                        )

                                    Err e ->
                                        ( ( Toy.Parser.mockRange, e ) :: inferenceErrors ++ errors
                                        , newEnvTypes
                                        , Dict.insert id exp expDict
                                        )

                            [] ->
                                ( inferenceErrors ++ errors
                                , newEnvTypes
                                , expDict
                                )
                else
                    ( annotationErrors ++ errors
                    , envTypes
                    , expDict
                    )

        _ ->
            ( ( mockRange, DifinitionDuplicated id ) :: errors
            , envTypes
            , expDict
            )


evalExpressionsForId :
    Dict String Type
    -> List ( Pos Expression, Type, Dependency )
    -> ( List Error, List ( Pos Expression, Type ) )
evalExpressionsForId envTypes expressions =
    expressions
        |> List.map (\( exp, t, dep ) -> ( exp, evalOneExpression envTypes t dep ))
        |> List.foldl
            (\( exp, result ) ( inferenceErrors, inferedTypes ) ->
                case result of
                    Ok t ->
                        ( inferenceErrors, ( exp, t ) :: inferedTypes )

                    Err errs ->
                        ( errs ++ inferenceErrors, inferedTypes )
            )
            ( [], [] )


evalOneExpression : Dict String Type -> Type -> Dependency -> Result (List Error) Type
evalOneExpression envTypes t dep =
    case resolveDependencies envTypes dep of
        ( [], env ) ->
            case evaluate env t of
                Ok ( t, env ) ->
                    Ok t

                Err e ->
                    Err [ e ]

        ( errors, _ ) ->
            Err errors


resolveDependencies : Dict String Type -> Dependency -> ( List Error, Env )
resolveDependencies envTypes dep =
    dep
        |> Dict.toList
        |> List.foldl
            (\( name, ( range, id ) ) ( errors, env ) ->
                case Dict.get name envTypes of
                    Just t ->
                        ( errors, Dict.insert id t env )

                    Nothing ->
                        ( ( range, VariableNotDefined name ) :: errors, env )
            )
            ( [], Dict.empty )


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


updateDefsForIdByStatement : Pos Statement -> ( Int, DefsForId ) -> ( Int, DefsForId )
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
