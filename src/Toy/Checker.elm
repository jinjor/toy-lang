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
    , expressions : List ( Type, Dependency )
    }


check : Module -> ( List Error, List Interface, List Implementation )
check module_ =
    let
        ( todo_handle_errors, expDict ) =
            getExpDict module_.statements

        ( n, envFromModule ) =
            expDict
                |> Dict.foldl
                    (\id exp ( n, dict ) ->
                        Typing.fromExp n Dict.empty exp
                            |> (\( t, n, dep ) ->
                                    ( n, Dict.insert id ( t, dep ) dict )
                               )
                    )
                    ( 0, Dict.empty )

        envTypes : Dict String Type
        envTypes =
            getTypeExpDict module_.statements
                |> Typing.fromTypeExpDict n

        typeAnnotationErrors =
            envTypes
                |> Dict.values
                |> List.concatMap (validateType Type.knownTypes)

        ( typeMatchingErrors, dict ) =
            checkAllTypes (Dict.toList envFromModule) envTypes
    in
        ( typeAnnotationErrors ++ typeMatchingErrors
        , dict
            |> Dict.toList
            |> List.map (\( id, t ) -> Interface id t)
        , expDict
            |> Dict.toList
            |> List.map (\( id, exp ) -> Implementation id exp)
        )


checkAllTypes :
    List ( String, ( Type, Dependency ) )
    -> Dict String Type
    -> ( List Error, Dict String Type )
checkAllTypes expList envTypes =
    case checkAllTypesHelp expList envTypes of
        Ok dict ->
            ( [], dict )

        Err errors ->
            ( errors, Dict.empty )


checkAllTypesHelp :
    List ( String, ( Type, Dependency ) )
    -> Dict String Type
    -> Result (List Error) (Dict String Type)
checkAllTypesHelp expList envTypes =
    case expList of
        [] ->
            Ok envTypes

        ( id, ( t, dep ) ) :: tail ->
            case resolveDependencies envTypes dep of
                ( [], env ) ->
                    evaluate env t
                        |> Result.mapError List.singleton
                        |> Result.andThen
                            (\( t, env ) ->
                                checkAllTypesHelp tail (Dict.insert id t envTypes)
                            )

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


getExpDict : List (Pos Statement) -> ( List Error, Dict String (Pos Expression) )
getExpDict statements =
    statements
        |> List.foldl
            (\statement ( errors, dict ) ->
                case statement.content of
                    Assignment id exp ->
                        case Dict.get id.content dict of
                            Just _ ->
                                ( ( id.range, DifinitionDuplicated id.content ) :: errors, dict )

                            Nothing ->
                                ( errors, Dict.insert id.content exp dict )

                    _ ->
                        ( errors, dict )
            )
            ( [], Dict.empty )


getTypeExpDict : List (Pos Statement) -> Dict String (Pos TypeExp)
getTypeExpDict statements =
    statements
        |> List.foldl
            (\statement dict ->
                case statement.content of
                    TypeSignature id exp ->
                        Dict.insert id exp dict

                    _ ->
                        dict
            )
            Dict.empty


collectDefs : List (Pos Statement) -> Defs
collectDefs statements =
    statements
        |> DictX.groupBy idOf
        |> Dict.foldl
            (\id statements ( n, dict ) ->
                let
                    ( n1, defsForId ) =
                        collectDefsForEachId n statements
                in
                    ( n1, Dict.insert id defsForId dict )
            )
            ( 0, Dict.empty )
        |> Tuple.second


collectDefsForEachId : Int -> List (Pos Statement) -> ( Int, DefsForId )
collectDefsForEachId n statements =
    statements
        |> List.foldl
            (\statement ( n, defsForId ) ->
                case statement.content of
                    Assignment id exp ->
                        let
                            ( t, n1, dep ) =
                                Typing.fromExp n Dict.empty exp
                        in
                            ( n1
                            , { defsForId
                                | expressions = defsForId.expressions ++ [ ( t, dep ) ]
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
            )
            ( n, DefsForId [] [] )


idOf : Pos Statement -> String
idOf statement =
    case statement.content of
        Assignment id _ ->
            id.content

        TypeSignature id _ ->
            id
