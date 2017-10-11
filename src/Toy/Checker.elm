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


type alias Error =
    ( Range, ErrorType )


type ErrorType
    = VariableNotDefined Identifier
    | VariableDuplicated Identifier
    | TypeNotDefined String
    | TypeDuplicated Identifier
    | TypeMismatch TypeExp TypeExp
    | TooManyArguments
    | NoImplementation Identifier
    | TypeSignatureMismatch TypeExp TypeExp


check : Module -> ( List Error, List Variable )
check module_ =
    let
        dict =
            makeVariables module_

        interfaces =
            addTypeUntilEnd dict
                |> Dict.values

        errors =
            collectErrors interfaces
    in
        ( errors, interfaces )


collectErrors : List Variable -> List Error
collectErrors variables =
    variables
        |> List.concatMap .errors


addTypeUntilEnd : Variables -> Variables
addTypeUntilEnd dict =
    dict
        |> Dict.values
        |> List.filter (\v -> v.errors == [])
        |> List.filter
            (\v ->
                case v.type_ of
                    Just ( _, True ) ->
                        False

                    _ ->
                        True
            )
        |> List.head
        |> Maybe.map (\v -> addTypeUntilEndHelp v dict |> addTypeUntilEnd)
        |> Maybe.withDefault dict


addTypeUntilEndHelp : Variable -> Variables -> Variables
addTypeUntilEndHelp v dict =
    let
        result =
            case v.exp of
                Just exp ->
                    lookupTypeForExpression dict exp
                        |> Result.andThen
                            (\( type_, newDict ) ->
                                case v.type_ of
                                    Nothing ->
                                        Ok ( type_, addCheckedType v type_ newDict )

                                    Just ( t, False ) ->
                                        if t == type_ then
                                            Ok ( type_, addCheckedType v type_ newDict )
                                        else
                                            Err ( exp.range, TypeSignatureMismatch t type_ )

                                    Just ( t, True ) ->
                                        Debug.crash "this should be already filtered out"
                            )

                Nothing ->
                    case v.type_ of
                        Just _ ->
                            Err ( Range (Position -1 -1) (Position -1 -1), NoImplementation v.id )

                        Nothing ->
                            Debug.crash "arienai"
    in
        case result of
            Err e ->
                dict
                    |> Dict.insert v.id { v | errors = e :: v.errors }

            Ok ( type_, newDict ) ->
                newDict
                    |> Dict.insert v.id { v | type_ = Just ( type_, True ) }


lookupTypeForInterface : Variables -> Variable -> Result Error ( TypeExp, Variables )
lookupTypeForInterface dict v =
    case ( v.type_, v.exp ) of
        ( Nothing, Just exp ) ->
            lookupTypeForExpression dict exp
                |> Result.map
                    (\( type_, newDict ) ->
                        ( type_, addCheckedType v type_ newDict )
                    )

        ( Just ( type_, _ ), _ ) ->
            Ok ( type_, dict )

        ( Nothing, Nothing ) ->
            Debug.crash "arienai"


applyType : TypeExp -> TypeExp -> Result ErrorType TypeExp
applyType t1 t2 =
    case t1 of
        ArrowType head tail ->
            if head == t2 then
                Ok tail
            else
                Err (TypeMismatch head t2)

        TypeValue _ _ ->
            Err TooManyArguments


lookupTypeForExpression : Variables -> Pos Expression -> Result Error ( TypeExp, Variables )
lookupTypeForExpression dict exp =
    case exp.content of
        NumberLiteral s ->
            Ok ( TypeValue "Number" [], dict )

        StringLiteral s ->
            Ok ( TypeValue "String" [], dict )

        Ref id tail ->
            lookupTypeForRef dict id exp.range
                |> Result.andThen
                    (\( type_, newDict ) ->
                        lookupTypeForExpressionTail newDict exp.range type_ tail
                    )


lookupTypeForRef : Variables -> Identifier -> Range -> Result Error ( TypeExp, Variables )
lookupTypeForRef dict id range =
    case Dict.get id dict of
        Nothing ->
            Err ( range, VariableNotDefined id )

        Just v ->
            lookupTypeForInterface dict v


lookupTypeForExpressionTail :
    Variables
    -> Range
    -> TypeExp
    -> List (Pos Expression)
    -> Result Error ( TypeExp, Variables )
lookupTypeForExpressionTail dict originalRange type_ tail =
    case tail of
        [] ->
            Ok ( type_, dict )

        firstArg :: tailArgs ->
            lookupTypeForExpression dict firstArg
                |> Result.andThen
                    (\( firstArgType, newDict ) ->
                        applyType type_ firstArgType
                            |> Result.mapError (\e -> ( originalRange, e ))
                            |> Result.andThen
                                (\nextType ->
                                    lookupTypeForExpressionTail newDict originalRange nextType tailArgs
                                )
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
                        updateByTypeSignature statement.range id typeExp dict
            )
            Dict.empty


updateByAssignment :
    Pos Identifier
    -> Pos Expression
    -> Variables
    -> Variables
updateByAssignment id exp dict =
    dict
        |> Dict.update id.content
            (\maybeVar ->
                case maybeVar of
                    Just old ->
                        if old.exp == Nothing then
                            Just { old | exp = Just exp }
                        else
                            Just { old | errors = ( id.range, VariableDuplicated id.content ) :: old.errors }

                    Nothing ->
                        Just <| Variable id.content Nothing (Just exp) []
            )


updateByTypeSignature :
    Range
    -> Identifier
    -> Pos TypeExp
    -> Variables
    -> Variables
updateByTypeSignature statementRange id typeExp dict =
    dict
        |> Dict.update id
            (\maybeVar ->
                case maybeVar of
                    Just old ->
                        if old.type_ == Nothing then
                            Just { old | type_ = Just ( typeExp.content, False ) }
                        else
                            Just { old | errors = ( statementRange, TypeDuplicated id ) :: old.errors }

                    Nothing ->
                        Just <| Variable id (Just ( typeExp.content, False )) Nothing []
            )
