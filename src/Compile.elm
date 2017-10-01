port module Compile exposing (..)

import Json.Decode as D
import Json.Encode as E
import Toy.Parser as ToyParser
import Toy.Checker as ToyChecker
import Parser
import Task


type alias Json =
    E.Value


main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- PORT


port parse : (String -> msg) -> Sub msg


port parsed : String -> Cmd msg


port checked : String -> Cmd msg


port err : String -> Cmd msg



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    {} ! []



-- UPDATE


type Msg
    = Parse String
    | Check ToyParser.Module


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse s ->
            case Parser.run (ToyParser.module_) s of
                Ok module_ ->
                    ( model
                    , Cmd.batch
                        [ (Task.succeed (Check module_)) |> Task.perform identity
                        , parsed (toString module_)
                        ]
                    )

                Err e ->
                    ( model, err (toString e) )

        Check module_ ->
            ( model
            , checked (toString <| ToyChecker.check module_)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ parse Parse
        ]
