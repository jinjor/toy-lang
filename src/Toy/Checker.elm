module Toy.Checker exposing (..)

import Toy.Position exposing (..)
import Toy.Parser exposing (..)
import Toy.Typing as Typing exposing (..)
import Toy.Type as Type exposing (..)
import Toy.Error exposing (..)
import Dict exposing (Dict)


type alias Interface =
    { id : String
    , type_ : Type
    }


type alias Implementation =
    { id : String
    , exp : Pos Expression
    }


check : Module -> ( List Error, List Interface, List Implementation )
check module_ =
    let
        expDict =
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

        envTypes =
            getTypeExpDict module_.statements
                |> Typing.fromTypeExpDict n

        result =
            checkAllTypes (Dict.toList envFromModule) envTypes
    in
        case result of
            Ok dict ->
                ( []
                , dict
                    |> Dict.toList
                    |> List.map (\( id, t ) -> Interface id t)
                , expDict
                    |> Dict.toList
                    |> List.map (\( id, exp ) -> Implementation id exp)
                )

            Err errors ->
                ( errors, [], [] )


checkAllTypes :
    List ( String, ( Type, Dependency ) )
    -> Dict String Type
    -> Result (List Error) (Dict String Type)
checkAllTypes expList envTypes =
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
                                checkAllTypes tail (Dict.insert id t envTypes)
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


getExpDict : List (Pos Statement) -> Dict String (Pos Expression)
getExpDict statements =
    statements
        |> List.foldl
            (\statement dict ->
                case statement.content of
                    Assignment id exp ->
                        Dict.insert id.content exp dict

                    _ ->
                        dict
            )
            Dict.empty


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
