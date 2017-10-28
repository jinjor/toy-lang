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
                |> Debug.log "resultDict"
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

            Err e ->
                ( [ e ], [], [] )


checkAllTypes : List ( String, ( Type, Dict String Int ) ) -> Dict String Type -> Result Error (Dict String Type)
checkAllTypes expList envTypes =
    case expList of
        [] ->
            Ok envTypes

        ( id, ( t, dep ) ) :: tail ->
            let
                env =
                    resolveDependencies envTypes dep
            in
                evaluate env t
                    |> Result.andThen
                        (\( t, env ) ->
                            checkAllTypes tail (Dict.insert id t envTypes)
                        )


resolveDependencies : Dict String Type -> Dict String Int -> Env
resolveDependencies envTypes dep =
    dep
        |> Dict.toList
        |> List.filterMap
            (\( name, id ) ->
                envTypes
                    |> Dict.get name
                    |> Maybe.map
                        (\t ->
                            ( id, t )
                        )
            )
        |> Dict.fromList


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
