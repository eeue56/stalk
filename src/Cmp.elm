module Cmp where

import Model exposing (..)
import Parser exposing (runtimeError)
import Stack

{-| 
eq args and push result
-}
eq : Argument -> Model -> Model 
eq args model = 
   case List.head args of
      Just a -> 
        case List.head <| List.drop 1 args of
          Just b -> Stack.push (toString <| a == b) model
          Nothing -> runtimeError ["Not enough items in args for eq!"] model
      Nothing -> runtimeError ["Not enough items in args for eq!"] model