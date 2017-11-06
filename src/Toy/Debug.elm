module Toy.Debug exposing (..)

import Dict exposing (Dict)


formatDict : (comparable -> String) -> (a -> String) -> Dict comparable a -> String
formatDict formatKey formatValue dict =
    "{ "
        ++ (dict
                |> Dict.toList
                |> List.map
                    (\( key, value ) ->
                        formatKey key ++ " => " ++ formatValue value
                    )
                |> String.join ", "
           )
        ++ " }"
