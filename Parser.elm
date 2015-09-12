module Parser where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)

compileError : Argument -> Model -> Model
compileError messages model =
  { model | errorMessage <- String.join "\n" <| (model.errorMessage) :: messages }

findCommand : String -> CommandLibrary -> Command
findCommand text dict =
  let
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
          Nothing -> CompileError ["command not found: " ++ trimText]
      else
        case List.head <| String.split "$" trimText of
          Just v -> case Dict.get (String.trim v) dict of
            Just command -> command <| List.map (String.trim) args 
            Nothing -> CompileError ["command not found: " ++ v]
          Nothing -> CompileError ["something up with this line: " ++ trimText]

parse : String -> Model -> (Command, Int)
parse someText model =
  if not <| String.startsWith "#" someText then (findCommand someText model.commands, 0)
    else
      let
        isHashAll = String.startsWith "#@" someText
        amount = 
          if isHashAll then List.length model.stack
          else
            List.length <| (String.indexes "#" someText) 
        tail = 
          if isHashAll then String.dropLeft 2 someText 
          else String.dropLeft amount someText
        args = 
            if (List.length model.stack) - amount < 0 then
              Nothing
            else
              Just <| log "erm" <| String.join "," <| List.take amount model.stack
        joiner = if String.contains "$" someText then ", " else " $ "
      in
        case args of 
          Just v -> (findCommand (log "commands" <| String.join "" [tail, joiner, v] ) model.commands, amount)
          Nothing -> log "Nothing at head!" (CompileError ["Not enough items on stack"], 0)
        