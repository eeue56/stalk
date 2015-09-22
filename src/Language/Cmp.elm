module Language.Cmp where

import Model exposing (..)
import Parser.Errors exposing (runtimeError)
import Language.Stack as Stack

cmpEngine : (String -> String -> Bool) -> Argument -> Model -> Model
cmpEngine cmp args model =
  case args of 
    a::b::[] -> Stack.push (toString <| cmp a b) model
    a::b::xs -> runtimeError ["Too many arguments for cmp!"] model
    a::[] -> runtimeError ["Only one argument provided.."] model
    [] -> runtimeError (["Not enough items in args for cmp!\nProvided:"] ++ args) model
    
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

true : Argument -> Model -> Model
true args model = 
  Stack.pushItem (List.all (\x -> x == "True") args) model

false : Argument -> Model -> Model
false args model = 
  case args of 
    [] -> Stack.push ("False") model
    xs -> Stack.pushItem (not <| List.any (\x -> x == "True") xs) model