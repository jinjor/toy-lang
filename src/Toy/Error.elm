module Toy.Error exposing (..)

import Toy.Position exposing (..)
import Toy.Type exposing (..)


type alias Error =
    ( Range, ErrorType )


type ErrorType
    = DifinitionDuplicated String
    | VariableNotDefined String
    | TypeNotDefined String
    | TypeMismatch Type Type
    | TooFewArguments
    | TooManyArguments
    | TooFewTypeArguments
    | TooManyTypeArguments
    | TypeSignatureMismatch Type Type
