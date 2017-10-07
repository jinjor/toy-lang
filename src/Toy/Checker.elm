module Toy.Checker exposing (..)

import Toy.Parser exposing (..)
import Toy.Formatter as Formatter
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


check : Module -> ( List String, List String )
check module_ =
    let
        dict =
            makeVariables module_

        typed =
            addTypeUntilEnd dict

        errors =
            collectErrors typed
                |> List.map (formatError "")

        interfaces =
            typed
                |> Dict.values
                |> List.map formatInterface
    in
        ( errors, interfaces )


formatInterface : Variable -> String
formatInterface v =
    Formatter.formatInterface v.id (Maybe.map Tuple.first v.type_)


formatError : String -> Error -> String
formatError source ( range, e ) =
    formatRange range ++ " " ++ formatErrorType e


formatRange : Range -> String
formatRange range =
    formatPosition range.start ++ " " ++ formatPosition range.end


formatPosition : Position -> String
formatPosition pos =
    toString pos.row ++ ":" ++ toString pos.col


formatErrorType : ErrorType -> String
formatErrorType e =
    case e of
        VariableNotDefined id ->
            id ++ " is not defined"

        VariableDuplicated id ->
            id ++ " is already defined"

        TypeNotDefined name ->
            "type " ++ name ++ "is not defined"

        TypeDuplicated id ->
            id ++ " is already typed"

        TypeMismatch expected actual ->
            "expected type "
                ++ Formatter.formatType expected
                ++ " but got type "
                ++ Formatter.formatType actual

        TooManyArguments ->
            "too many arguments"


collectErrors : Variables -> List Error
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
                case lookupType dict v.id (Range (Position -1 -1) (Position -1 -1)) [] of
                    Err e ->
                        dict
                            |> Dict.insert v.id
                                { v | errors = e :: v.errors }
                            |> addTypeUntilEnd

                    Ok ( typeExp, newDict ) ->
                        newDict
                            |> Dict.insert v.id { v | type_ = Just ( typeExp, True ) }
                            |> addTypeUntilEnd

            _ ->
                dict


lookupType : Variables -> Identifier -> Range -> List (Pos Expression) -> Result Error ( TypeExp, Variables )
lookupType dict id range tail =
    case Dict.get id dict of
        Nothing ->
            Err ( range, VariableNotDefined id )

        Just v ->
            let
                result =
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
            in
                result
                    |> Result.andThen
                        (\( type_, newDict ) ->
                            lookupTypeForExpressions newDict type_ tail
                        )


lookupTypeForExpressions :
    Variables
    -> TypeExp
    -> List (Pos Expression)
    -> Result Error ( TypeExp, Variables )
lookupTypeForExpressions dict type_ tail =
    case tail of
        [] ->
            Ok ( type_, dict )

        firstArg :: tailArgs ->
            lookupTypeForExpression dict firstArg
                |> Result.andThen
                    (\( firstArgType, newDict ) ->
                        applyType type_ firstArgType
                            |> Result.mapError (\e -> ( Range (Position -2 -2) (Position -2 -2), e ))
                            |> Result.andThen
                                (\nextType ->
                                    lookupTypeForExpressions newDict nextType tailArgs
                                )
                    )


applyType : TypeExp -> TypeExp -> Result ErrorType TypeExp
applyType t1 t2 =
    case t1 of
        ArrowType head tail ->
            case tail of
                Just t ->
                    if head == t2 then
                        Ok t
                    else
                        Err (TypeMismatch head t2)

                Nothing ->
                    Err TooManyArguments

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
            lookupType dict id exp.range tail


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
                            Just <| { old | exp = Just exp }
                        else
                            Just <| { old | errors = ( id.range, VariableDuplicated id.content ) :: old.errors }

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
                            Just <| { old | type_ = Just ( typeExp.content, False ) }
                        else
                            Just <| { old | errors = ( statementRange, TypeDuplicated id ) :: old.errors }

                    Nothing ->
                        Just <| Variable id (Just ( typeExp.content, False )) Nothing []
            )
