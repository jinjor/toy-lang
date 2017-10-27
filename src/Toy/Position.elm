module Toy.Position exposing (..)


type alias Position =
    { row : Int
    , col : Int
    }


type alias Range =
    { start : Position
    , end : Position
    }


type alias Pos a =
    { range : Range
    , content : a
    }
