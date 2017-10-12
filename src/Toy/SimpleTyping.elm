module Toy.SimpleTyping exposing (..)

import Dict exposing (Dict)


type Exp
    = Ref String
    | IntLiteral
    | StringLiteral
    | Lambda String Exp
    | Call Exp Exp


{-|
  01: 1
  02: ""
  03: a
  04: f 1
  05: f 1 ""
  06: f a
  07: \a -> 1
  08: \a -> a
  09: \a -> increment a
  10: \a -> add a 1
  11: \a -> \b -> 1
  12: \a -> \b -> a
-}
examples =
    { e01 = IntLiteral
    , e02 = StringLiteral
    , e03 = Ref "a"
    , e04 = Call (Ref "f") IntLiteral
    , e05 = Call (Call (Ref "f") IntLiteral) StringLiteral
    , e06 = Call (Ref "f") (Ref "a")
    , e07 = Lambda "a" IntLiteral
    , e08 = Lambda "a" (Ref "a")
    , e09 = Lambda "a" (Call (Ref "increment") IntLiteral)
    , e10 = Lambda "a" (Call (Call (Ref "add") (Ref "a")) IntLiteral)
    , e11 = Lambda "a" (Lambda "b" IntLiteral)
    , e12 = Lambda "a" (Lambda "b" (Ref "a"))
    }


type alias Env =
    Dict String Type


type Type
    = TypeVar String
    | TypeValue String
    | TypeLambda String Type


calc : Exp -> Type
calc =
    Debug.crash "not implemented"


evaluate : Type -> Env -> Type
evaluate =
    Debug.crash "not implemented"
