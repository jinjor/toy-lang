module Toy.Checker exposing (..)

import Toy.Parser exposing (..)
import Dict exposing (Dict)


type alias Variable =
    { id : Identifier
    , type_ : Maybe ( TypeExp, Bool )
    , exp : Maybe (Pos Expression)
    , errors : List Error
    }


type Variables
    = Variables (Dict Identifier Variable) (Maybe Variables)


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
            makeVarDict module_

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


addTypeUntilEnd : Dict Identifier Variable -> Dict Identifier Variable
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


addTypeUntilEndHelp : Variable -> Dict Identifier Variable -> Dict Identifier Variable
addTypeUntilEndHelp v dict =
    let
        result =
            case v.exp of
                Just exp ->
                    lookupTypeForExpression (Variables dict Nothing) exp
                        |> Result.andThen
                            (\( type_, newVars ) ->
                                case v.type_ of
                                    Nothing ->
                                        Ok ( type_, addCheckedType v type_ newVars )

                                    Just ( t, False ) ->
                                        if t == type_ then
                                            Ok ( type_, addCheckedType v type_ newVars )
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
                    |> addCheckedType v type_
                    |> (\(Variables dict _) -> dict)


lookupTypeForInterface : Variables -> Variable -> Result Error ( TypeExp, Variables )
lookupTypeForInterface vars v =
    case ( v.type_, v.exp ) of
        ( Nothing, Just exp ) ->
            lookupTypeForExpression vars exp
                |> Result.map
                    (\( type_, newVars ) ->
                        ( type_, addCheckedType v type_ newVars )
                    )

        ( Just ( type_, _ ), _ ) ->
            Ok ( type_, vars )

        ( Nothing, Nothing ) ->
            Debug.crash "arienai"


lookupTypeForExpression : Variables -> Pos Expression -> Result Error ( TypeExp, Variables )
lookupTypeForExpression vars exp =
    case exp.content of
        NumberLiteral s ->
            Ok ( TypeValue "Number" [], vars )

        StringLiteral s ->
            Ok ( TypeValue "String" [], vars )

        Lambda argName exp ->
            lookupTypeForExpression vars exp

        Call first next ->
            lookupTypeForExpression dict first
                |> Result.andThen
                    (\( funcType, dict1 ) ->
                        lookupTypeForExpression dict1 next
                            |> Result.andThen
                                (\( argType, dict2 ) ->
                                    applyType funcType argType
                                        |> Result.mapError (\e -> ( exp.range, e ))
                                        |> Result.map (\t -> ( t, dict2 ))
                                )
                    )

        Ref id ->
            lookupTypeForRef dict id exp.range


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


lookupTypeForRef : Variables -> Identifier -> Range -> Result Error ( TypeExp, Variables )
lookupTypeForRef (Variables dict tail) id range =
    case Dict.get id dict of
        Nothing ->
            case tail of
                Just vars ->
                    lookupTypeForRef vars id range
                        |> Result.map
                            (Tuple.mapSecond
                                (\newTail ->
                                    Variables dict (Just newTail)
                                )
                            )

                Nothing ->
                    Err ( range, VariableNotDefined id )

        Just v ->
            lookupTypeForInterface (Variables dict tail) v


addCheckedType : Variable -> TypeExp -> Variables -> Variables
addCheckedType v type_ (Variables dict tail) =
    Variables
        (dict
            |> Dict.insert v.id { v | type_ = Just ( type_, True ) }
        )
        tail


makeVarDict : Module -> Dict Identifier Variable
makeVarDict (Module statements) =
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
    -> Dict Identifier Variable
    -> Dict Identifier Variable
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
    -> Dict Identifier Variable
    -> Dict Identifier Variable
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
