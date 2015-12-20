module Language.Strings where

import Model exposing (..)
import Parser.Errors exposing (runtimeError)
import Language.Stack as Stack
import String

toStack : Argument -> Model -> Model
toStack args model =
    case args of
        [] -> runtimeError ["Not enough arguments for to-stack"] model
        x::_ -> Stack.pushManyStrings (List.map String.fromChar <| String.toList x) model
