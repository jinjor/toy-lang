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
            makeVarDictFromModule module_

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
                            Debug.crash "top level variables must have either type or expression"
    in
        case result of
            Err e ->
                dict
                    |> Dict.insert v.id { v | errors = e :: v.errors }

            Ok ( type_, newDict ) ->
                newDict
                    |> addCheckedType v type_
                    |> (\(Variables dict _) -> dict)


lookupTypeForExpression : Variables -> Pos Expression -> Result Error ( TypeExp, Variables )
lookupTypeForExpression vars exp =
    case exp.content of
        NumberLiteral s ->
            Ok ( TypeValue "Number" [], vars )

        StringLiteral s ->
            Ok ( TypeValue "String" [], vars )

        Lambda patterns exp ->
            let
                localDict =
                    makeVarDict (patternsToNames patterns) []
            in
                lookupTypeForExpression (Variables localDict (Just vars)) exp

        Call first next ->
            lookupTypeForExpression vars first
                |> Result.andThen
                    (\( funcType, vars1 ) ->
                        lookupTypeForExpression vars1 next
                            |> Result.andThen
                                (\( argType, vars2 ) ->
                                    applyType funcType argType
                                        |> Result.mapError (\e -> ( exp.range, e ))
                                        |> Result.map (\t -> ( t, vars2 ))
                                )
                    )

        Ref id ->
            lookupTypeForRef vars id exp.range


patternsToNames : Patterns -> List String
patternsToNames (Patterns pattern tail) =
    pattern
        :: (case tail of
                Just t ->
                    patternsToNames t

                Nothing ->
                    []
           )


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
            case ( v.type_, v.exp ) of
                ( Nothing, Just exp ) ->
                    lookupTypeForExpression (Variables dict tail) exp
                        |> Result.map
                            (\( type_, newVars ) ->
                                ( type_, addCheckedType v type_ newVars )
                            )

                ( Just ( type_, _ ), _ ) ->
                    Ok ( type_, (Variables dict tail) )

                ( Nothing, Nothing ) ->
                    Debug.crash "not implemented yet -- this is one of lambda args"


addCheckedType : Variable -> TypeExp -> Variables -> Variables
addCheckedType v type_ (Variables dict tail) =
    Variables
        (dict
            |> Dict.insert v.id { v | type_ = Just ( type_, True ) }
        )
        tail


makeVarDictFromModule : Module -> Dict Identifier Variable
makeVarDictFromModule (Module statements) =
    makeVarDict [] statements


makeVarDict : List String -> List (Pos Statement) -> Dict Identifier Variable
makeVarDict argNames statements =
    statements
        |> List.foldl
            (\statement dict ->
                case statement.content of
                    Assignment id exp ->
                        updateByAssignment id exp dict

                    TypeSignature id typeExp ->
                        updateByTypeSignature statement.range id typeExp dict
            )
            (Dict.fromList
                (List.map
                    (\id ->
                        ( id, Variable id Nothing Nothing [] )
                    )
                    argNames
                )
            )


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
