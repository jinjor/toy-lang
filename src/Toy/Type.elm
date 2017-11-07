module Toy.Type exposing (Type(..), TypeDef, knownTypes)

import Toy.Position exposing (..)
import Dict exposing (Dict)


type Type
    = TypeVar Int
    | TypeValue String (List Type)
    | TypeArrow Type Type
    | TypeApply Range Type Type


type alias TypeDef =
    { name : String
    , arity : Int
    }


int : TypeDef
int =
    TypeDef "Int" 0


string : TypeDef
string =
    TypeDef "String" 0


bool : TypeDef
bool =
    TypeDef "Bool" 0


list : TypeDef
list =
    TypeDef "List" 1


knownTypes : Dict String TypeDef
knownTypes =
    [ int, string, bool, list ]
        |> List.map (\def -> ( def.name, def ))
        |> Dict.fromList
