module Toy.Type exposing (..)

import Toy.Position exposing (..)


type Type
    = TypeVar Int
    | TypeValue String (List Type)
    | TypeArrow Type Type
    | TypeApply Range Type Type
