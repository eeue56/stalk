module Cmp where

import Model exposing (..)
import Parser.Errors exposing (runtimeError)
import Stack

cmpEngine : (String -> String -> Bool) -> Argument -> Model -> Model
cmpEngine cmp args model = 
  case List.head args of
    Just a -> 
      case List.head <| List.drop 1 args of
        Just b -> Stack.push (toString <| cmp a b) model
        Nothing -> runtimeError ["Not enough items in args for cmp!"] model
    Nothing -> runtimeError ["Not enough items in args for cmp!"] model

{-| 
eq args and push result
-}
eq : Argument -> Model -> Model 
eq = cmpEngine (==)

{-| 
eq args and push result
-}
notEq : Argument -> Model -> Model 
notEq = cmpEngine (/=)

{-| 
less than args and push result
-}
lessThan : Argument -> Model -> Model 
lessThan = cmpEngine (<)

{-| 
less than args and push result
-}
lessThanOrEquals : Argument -> Model -> Model 
lessThanOrEquals = cmpEngine (<=)

{-| 
more than args and push result
-}
moreThan : Argument -> Model -> Model 
moreThan = cmpEngine (>)

{-| 
more than args and push result
-}
moreThanOrEquals : Argument -> Model -> Model 
moreThanOrEquals = cmpEngine (>=)