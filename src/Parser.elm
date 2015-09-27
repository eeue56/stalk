module Parser where

import String

import Model exposing (..)
import Parser.Symbols as Symbols
import Parser.Parse exposing (..)
import Debug exposing (log)

isLabel : String -> Bool
isLabel line =
  String.startsWith Symbols.label line

isStackOp : String -> Bool
isStackOp line =
  String.startsWith Symbols.stackPop line || String.startsWith Symbols.stackUse line

isComment : String -> Bool
isComment line =
  String.startsWith Symbols.comment line

{- 
 Parse takes a string to parse and a model to use for finding commands,
 returning a Command with the params given and the number of stack uses
 as a tuple
    -}
parse : String -> Model -> (Command, Int)
parse line model =
  if isComment line then (Still, 0)
  else
    if isLabel line then (Label [String.dropLeft 1 line], 0)
    else
      if not <| isStackOp line then (findCommand line model.commands, 0)
      else 
        (if String.startsWith Symbols.stackPop line then parseStackPop line model
         else parseStackUse line model)
