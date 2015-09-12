module Parser where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)

compileError : Argument -> Model -> Model
compileError messages model =
  { model | errorMessage <- String.join "\n" <| (model.errorMessage) :: messages }

commandNotFound : CommandLibrary -> String -> Command
commandNotFound dict command  =
  let
    helpWarning = 
      if (List.length <| String.split " " command) > 2 then "\nMaybe you forgot a $?"
      else ""
  in
    CompileError ["command not found: " ++ command ++ helpWarning]

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

consumesWholeStack : String -> Bool
consumesWholeStack line =
  String.startsWith "#@" line

stackPopCount : Bool -> String -> Model -> Int
stackPopCount consumesWhole line model =
  if consumesWhole then List.length model.stack
  else List.length <| (String.indexes "#" line) 

stripStackOperations : Bool -> String -> Int -> String
stripStackOperations consumesWhole line amount =
  if consumesWhole then String.dropLeft 2 line
  else String.dropLeft amount line 
        
parseStackPop : String -> Model -> (Command, Int)
parseStackPop line model =
  let
      isHashAll = consumesWholeStack line
      amount = stackPopCount isHashAll line model
      tail = stripStackOperations isHashAll line amount
      args = 
          if (List.length model.stack) - amount < 0 then Nothing
          else Just <| log "erm" <| String.join "," <| List.take amount model.stack
      joiner = if String.contains "$" line then ", " else " $ "
    in
      case args of 
        Just v -> (findCommand (log "commands" <| String.join "" [tail, joiner, v] ) model.commands, amount)
        Nothing -> log "Nothing at head!" (CompileError ["Not enough items on stack"], 0)

parse : String -> Model -> (Command, Int)
parse line model =
  if not <| String.startsWith "#" line then (findCommand line model.commands, 0)
  else parseStackPop line model