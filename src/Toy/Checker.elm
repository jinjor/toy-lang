module Toy.Checker exposing (..)

import Toy.Parser exposing (..)
import Dict exposing (Dict)


type alias Variable =
    { id : Identifier
    , type_ : Maybe ( TypeExp, Bool )
    , exp : Maybe (Pos Expression)
    , errors : List Error
    }


type alias Variables =
    Dict Identifier Variable


type alias Range =
    { start : ( Int, Int )
    , end : ( Int, Int )
    }


type alias Error =
    ( Range, String )


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

        interfaces =
            typed
                |> Dict.values
                |> List.map formatInterface
                |> List.map (Debug.log "type")
    in
        typed


formatInterface : Variable -> String
formatInterface v =
    let
        typeString =
            case v.type_ of
                Just ( t, _ ) ->
                    formatType t

                Nothing ->
                    "?"
    in
        v.id ++ " : " ++ typeString


formatType : TypeExp -> String
formatType type_ =
    case type_ of
        ArrowType head tail ->
            case tail of
                Just t ->
                    formatType head ++ " -> " ++ formatType t

                Nothing ->
                    formatType head

        AtomType name ->
            name


formatError : String -> ( Range, String ) -> String
formatError source ( range, e ) =
    toString (Tuple.first range.start)
        ++ ":"
        ++ toString (Tuple.second range.start)
        ++ " "
        ++ e


collectErrors : Variables -> List ( Range, String )
collectErrors typedDict =
    typedDict
        |> Dict.values
        |> List.concatMap .errors


addTypeUntilEnd : Variables -> Variables
addTypeUntilEnd dict =
    let
        target =
            dict
                |> Dict.filter (\key v -> v.type_ == Nothing && v.errors == [])
                |> Dict.values
                |> List.head
    in
        case target of
            Just v ->
                case lookupType dict v.id [] of
                    Err e ->
                        dict
                            |> Dict.insert v.id
                                { v | errors = ( Range ( -1, -1 ) ( -1, 1 ), e ) :: v.errors }
                            |> addTypeUntilEnd

                    Ok ( typeExp, newDict ) ->
                        newDict
                            |> Dict.insert v.id { v | type_ = Just ( typeExp, True ) }
                            |> addTypeUntilEnd

            _ ->
                dict


lookupType : Variables -> Identifier -> List (Pos Expression) -> Result String ( TypeExp, Variables )
lookupType dict id tail =
    case Dict.get id dict of
        Nothing ->
            Err (id ++ " is not defined")

        Just v ->
            let
                result =
                    case ( v.type_, v.exp ) of
                        ( Nothing, Just exp ) ->
                            lookupTypeForExpression dict v exp.content

                        ( Just ( type_, _ ), _ ) ->
                            Ok ( type_, dict )

                        ( Nothing, Nothing ) ->
                            Debug.crash "arienai"
            in
                result
                    |> Result.andThen
                        (\( type_, newDict ) ->
                            lookupTypeForExpressions newDict v type_ tail
                        )


lookupTypeForExpressions :
    Variables
    -> Variable
    -> TypeExp
    -> List (Pos Expression)
    -> Result String ( TypeExp, Variables )
lookupTypeForExpressions dict v type_ tail =
    case tail of
        [] ->
            Ok ( type_, dict )

        firstArg :: tailArgs ->
            lookupTypeForExpression dict (makeLocalVariableMock firstArg) firstArg.content
                |> Result.andThen
                    (\( firstArgType, newDict ) ->
                        applyType type_ firstArgType
                            |> Result.andThen
                                (\nextType ->
                                    lookupTypeForExpressions newDict v nextType tailArgs
                                )
                    )


makeLocalVariableMock : Pos Expression -> Variable
makeLocalVariableMock exp =
    Variable "__local__" Nothing (Just exp) []


applyType : TypeExp -> TypeExp -> Result String TypeExp
applyType t1 t2 =
    case t1 of
        ArrowType head tail ->
            case tail of
                Just t ->
                    if head == t2 then
                        Ok t
                    else
                        Err "type mismatch"

                Nothing ->
                    Err "too many arguments"

        AtomType name ->
            Err "too many arguments"


lookupTypeForExpression : Variables -> Variable -> Expression -> Result String ( TypeExp, Variables )
lookupTypeForExpression dict v exp =
    case exp of
        NumberLiteral s ->
            let
                type_ =
                    AtomType "Number"
            in
                Ok ( type_, addCheckedType v type_ dict )

        StringLiteral s ->
            let
                type_ =
                    AtomType "String"
            in
                Ok ( type_, addCheckedType v type_ dict )

        Ref id tail ->
            lookupType dict id tail
                |> Result.map
                    (\( type_, newDict ) ->
                        ( type_, addCheckedType v type_ newDict )
                    )


addCheckedType : Variable -> TypeExp -> Variables -> Variables
addCheckedType v type_ dict =
    dict
        |> Dict.insert v.id { v | type_ = Just ( type_, True ) }


makeVariables : Module -> Variables
makeVariables (Module statements) =
    statements
        |> List.foldl
            (\statement dict ->
                case statement.content of
                    Assignment id exp ->
                        updateByAssignment id exp dict

                    TypeSignature id typeExp ->
                        updateByTypeSignature id typeExp dict
            )
            Dict.empty


updateByAssignment :
    Identifier
    -> Pos Expression
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
                            Just <| { old | errors = ( Range exp.start exp.end, id ++ " is already defined." ) :: old.errors }

                    Nothing ->
                        Just <| Variable id Nothing (Just exp) []
            )


updateByTypeSignature :
    Identifier
    -> Pos TypeExp
    -> Variables
    -> Variables
updateByTypeSignature id typeExp dict =
    dict
        |> Dict.update id
            (\maybeVar ->
                case maybeVar of
                    Just old ->
                        if old.type_ == Nothing then
                            Just <| { old | type_ = Just ( typeExp.content, False ) }
                        else
                            Just <| { old | errors = ( Range typeExp.start typeExp.end, id ++ " is already defined." ) :: old.errors }

                    Nothing ->
                        Just <| Variable id (Just ( typeExp.content, False )) Nothing []
            )
