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

parse : String -> Model -> (Command, Bool)
parse someText model =
  if not <| String.startsWith "#" someText then (findCommand someText model.commands, False)
    else
      let
        tail = String.dropLeft 1 someText
        args = 
          let
            amount = List.length <| (String.indexes "#" someText) 
          in
            if amount - (List.length model.stack) < 0 then
              Nothing
            else
              Just <| String.join "," <| List.take amount model.stack
        joiner = if String.contains "$" someText then ", " else " $ "
      in
        case args of 
          Just v -> (findCommand (String.join "" [tail, joiner, v] ) model.commands, True)
          Nothing -> log "Nothing at head!" (Failed, False)
        