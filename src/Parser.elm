module Parser where

import String

import Model exposing (..)
import Parser.Symbols as Symbols
import Parser.Parse exposing (..)
import Debug exposing (log)


isStackOp : String -> Bool
isStackOp line =
  String.startsWith Symbols.stackPop line || String.startsWith Symbols.stackUse line

isComment : String -> Bool
isComment line =
  String.startsWith Symbols.comment line

parse : String -> Model -> (Command, Int)
parse line model =
  if isComment line then (Still, 0)
  else
    if not <| isStackOp line then (findCommand line model.commands, 0)
    else 
      (if String.startsWith Symbols.stackPop line then parseStackPop line model
       else parseStackUse line model)
