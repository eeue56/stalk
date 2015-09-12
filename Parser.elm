module Parser where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)

findCommand : String -> CommandLibrary -> Command
findCommand text dict =
  let
    --commands = List.map (String.trim) <| String.split " " text
    trimText = String.trim text --List.take 1 commands
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
          Nothing -> Failed
      else
        case List.head <| String.split "$" trimText of
          Just v -> case Dict.get (String.trim v) dict of
            Just command -> command <| List.map (String.trim) args 
            Nothing -> Failed
          Nothing -> Failed

parse : String -> Model -> (Command, Int)
parse someText model =
  if not <| String.startsWith "#" someText then (findCommand someText model.commands, 0)
    else
      let
        amount = List.length <| (String.indexes "#" someText) 
        tail = String.dropLeft amount someText
        args = 
            if amount - (List.length model.stack) < 0 then
              Nothing
            else
              Just <| log "erm" <| String.join "," <| List.take amount model.stack
        joiner = if String.contains "$" someText then ", " else " $ "
      in
        case args of 
          Just v -> (findCommand (log "commands" <| String.join "" [tail, joiner, v] ) model.commands, amount)
          Nothing -> log "Nothing at head!" (Failed, 0)
        