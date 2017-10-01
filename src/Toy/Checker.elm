module Toy.Checker exposing (..)

import Toy.Parser exposing (..)
import Dict exposing (Dict)


type alias Variable =
    { id : Identifier
    , type_ : Maybe ( TypeName, Bool )
    , exp : Maybe (Positioned Expression)
    , error : Maybe String
    }


type alias Variables =
    Dict Identifier Variable


check : Module -> Variables
check module_ =
    let
        dict =
            makeVariables module_

        typed =
            addTypeUntilEnd dict

        errors =
            collectErrors typed
                |> List.map (formatError "")
                |> List.map (Debug.log "error")
    in
        typed


formatError : String -> Positioned String -> String
formatError source e =
    toString (Tuple.first e.start)
        ++ ":"
        ++ toString (Tuple.second e.start)
        ++ " "
        ++ e.content


collectErrors : Variables -> List (Positioned String)
collectErrors typedDict =
    typedDict
        |> Dict.values
        |> List.filterMap collectError


collectError : Variable -> Maybe (Positioned String)
collectError v =
    case ( v.exp, v.error ) of
        ( Just exp, Just e ) ->
            Just (Positioned exp.start exp.end e)

        ( Nothing, Just e ) ->
            Just (Positioned ( -1, -1 ) ( -1, -1 ) e)

        _ ->
            Nothing


addTypeUntilEnd : Variables -> Variables
addTypeUntilEnd dict =
    let
        target =
            dict
                |> Dict.filter (\key v -> v.type_ == Nothing && v.error == Nothing)
                |> Dict.values
                |> List.head
    in
        case target of
            Just v ->
                case lookupType dict v.id of
                    Err e ->
                        dict
                            |> Dict.insert v.id
                                { v
                                    | error = Just e
                                }
                            |> addTypeUntilEnd

                    Ok ( typeName, newDict ) ->
                        newDict
                            |> Dict.insert v.id { v | type_ = Just ( typeName, True ) }
                            |> addTypeUntilEnd

            _ ->
                dict


lookupType : Variables -> Identifier -> Result String ( TypeName, Variables )
lookupType dict id =
    case Dict.get id dict of
        Nothing ->
            Err (id ++ " is not defined")

        Just v ->
            case ( v.type_, v.exp ) of
                ( Nothing, Just exp ) ->
                    case exp.content of
                        NumberLiteral s ->
                            Ok
                                ( "Number"
                                , dict
                                    |> Dict.insert v.id { v | type_ = Just ( "Number", True ) }
                                )

                        StringLiteral s ->
                            Ok
                                ( "String"
                                , dict
                                    |> Dict.insert v.id { v | type_ = Just ( "String", True ) }
                                )

                        Ref id ->
                            lookupType dict id
                                |> Result.map
                                    (\( typeName, newDict ) ->
                                        ( typeName
                                        , dict
                                            |> Dict.insert v.id { v | type_ = Just ( typeName, True ) }
                                        )
                                    )

                ( Just ( typeName, _ ), _ ) ->
                    Ok ( typeName, dict )

                ( Nothing, Nothing ) ->
                    Debug.crash "arienai"


makeVariables : Module -> Variables
makeVariables (Module statements) =
    statements
        |> List.foldl
            (\statement dict ->
                case statement.content of
                    Assignment id exp ->
                        updateByAssignment id exp dict

                    TypeSignature id typeName ->
                        updateByTypeSignature id typeName dict
            )
            Dict.empty


updateByAssignment :
    Identifier
    -> Positioned Expression
    -> Variables
    -> Variables
updateByAssignment id exp dict =
    dict
        |> Dict.update id
            (\maybeVar ->
                case maybeVar of
                    Just old ->
                        if old.exp == Nothing then
                            Just <| { old | exp = Just exp }
                        else
                            Just <| { old | error = Just (id ++ " is already defined.") }

                    Nothing ->
                        Just <| Variable id Nothing (Just exp) Nothing
            )


updateByTypeSignature :
    Identifier
    -> TypeName
    -> Variables
    -> Variables
updateByTypeSignature id typeName dict =
    dict
        |> Dict.update id
            (\maybeVar ->
                case maybeVar of
                    Just old ->
                        if old.type_ == Nothing then
                            Just <| { old | type_ = Just ( typeName, False ) }
                        else
                            Just <| { old | error = Just (id ++ " is already typed.") }

                    Nothing ->
                        Just <| Variable id (Just ( typeName, False )) Nothing Nothing
            )
