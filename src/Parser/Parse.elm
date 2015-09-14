module Parser.Parse where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)

import Parser.Errors exposing (..)

{-|
takes text, tries to extract command and args, then return them
-}
findCommand : String -> CommandLibrary -> Command
findCommand text dict =
  let
    commandNotFound' = commandNotFound dict
    trimText = String.trim text
    hasArgs = String.contains "$" trimText
    args = if not hasArgs then [] else
      case List.tail <| String.split "$" trimText of 
        Just v -> case List.head v of 
          Just x -> String.split "," x
          Nothing -> []
        Nothing -> []
  in
    if not hasArgs 
      then 
        case Dict.get trimText dict of
          Just v -> v []
          Nothing -> commandNotFound' trimText
      else
        case List.head <| String.split "$" trimText of
          Just v -> case Dict.get (String.trim v) dict of
            Just command -> command <| List.map (String.trim) args 
            Nothing -> commandNotFound' v
          Nothing -> CompileError ["something up with this line: " ++ trimText]

{-|
returns true if the line starts with the @ symbol
-}
consumesWholeStack : String -> Bool
consumesWholeStack line =
  String.startsWith "@" line

{-|
the amount of stack operations
-}
stackOpCount : String -> Bool -> String -> Model -> Int
stackOpCount op consumesWhole line model =
  if consumesWhole then List.length model.stack
  else List.length <| (String.indexes op line) 


stackPopCount = stackOpCount "#" 
stackUseCount = stackOpCount ">"

{-|
remove stack operations from the front of the line
-}
stripStackOperations : Bool -> String -> Int -> String
stripStackOperations consumesWhole line amount =
  if consumesWhole then String.dropLeft 2 line
  else String.dropLeft amount line 


parseStacking : String -> Model -> Bool -> Int -> Int -> (Command, Int)
parseStacking line model isAll amountOfOps ops =
  let
    tail = stripStackOperations isAll line amountOfOps
    args = 
          if (List.length model.stack) - amountOfOps < 0 then Nothing
          else Just <| log "erm" <| String.join "," <| List.take amountOfOps model.stack
    joiner = if String.contains "$" tail then ", " else " $ "
  in
    case args of 
      Just v -> 
        (findCommand (log "commands" <| String.join "" [tail, joiner, v] ) model.commands, ops)
      Nothing -> 
        log "Nothing at head!" (CompileError ["Not enough items on stack with: " ++ line], 0)

{-|
  take a line, get the stack items, find the command and send the stack items as arguments
-}        
parseStackPop : String -> Model -> (Command, Int)
parseStackPop line model =
  let
    isAll = consumesWholeStack <| String.dropLeft 1 line
    amount = stackPopCount isAll line model
  in
    parseStacking line model isAll amount amount

{-|
  take a line, get the stack items, find the command and send the stack items as arguments
-}   
parseStackUse : String -> Model -> (Command, Int)
parseStackUse line model =
  let
    isAll = consumesWholeStack <| String.dropLeft 1 line
    amount = stackUseCount isAll line model
  in
    parseStacking line model isAll amount 0