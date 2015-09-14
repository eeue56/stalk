module Parser where

import String

import Model exposing (..)
import Parser.Parse exposing (..)


isStackOp : String -> Bool
isStackOp line =
  String.startsWith "#" line || String.startsWith ">" line

isComment : String -> Bool
isComment line =
  String.startsWith ";" line

parse : String -> Model -> (Command, Int)
parse line model =
  if isComment line then (Still, 0)
  else
    if not <| isStackOp line then (findCommand line model.commands, 0)
    else 
      (if String.startsWith "#" line then parseStackPop line model
       else parseStackUse line model)