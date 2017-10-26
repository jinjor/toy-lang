module Toy.Checker2 exposing (..)

import Toy.Parser exposing (..)
import Toy.SimpleTyping as SimpleTyping exposing (..)
import Dict exposing (Dict)


type alias Interface =
    { id : String
    , type_ : TypeExp
    }


type Error
    = TypeError SimpleTyping.Error


check : Module -> ( List Error, List Interface )
check module_ =
    let
        ( n, expDict ) =
            getExpDict module_
                |> Dict.foldl
                    (\id exp ( n, dict ) ->
                        SimpleTyping.fromExp n Dict.empty exp
                            |> (\( t, n, dep ) ->
                                    ( n, Dict.insert id ( t, dep ) dict )
                               )
                    )
                    ( 0, Dict.empty )

        envTypes =
            getTypeExpDict module_
                |> Dict.map
                    (\id tExp ->
                        SimpleTyping.fromTypeExp
                            (SimpleTyping.initFromTypeExpState n)
                            tExp.content
                            |> Tuple.first
                    )

        resultDict =
            checkAllTypes (Dict.toList expDict) envTypes
                |> Debug.log "resultDict"
    in
        ( [], [] )


checkAllTypes : List ( String, ( Type, Dict String Int ) ) -> Dict String Type -> Dict String Type
checkAllTypes expList envTypes =
    case expList of
        [] ->
            envTypes

        ( id, ( t, dep ) ) :: tail ->
            let
                env =
                    resolveDependencies envTypes dep
            in
                case evaluate env t of
                    Ok ( t, env ) ->
                        checkAllTypes tail (Dict.insert id t envTypes)

                    Err ( range, e ) ->
                        let
                            _ =
                                Debug.log "error" e
                        in
                            envTypes


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


getExpDict : Module -> Dict String (Pos Expression)
getExpDict (Module statements) =
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


getTypeExpDict : Module -> Dict String (Pos TypeExp)
getTypeExpDict (Module statements) =
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
