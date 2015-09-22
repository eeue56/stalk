module Parser.Errors where

import String
import Dict
import Model exposing (..)
import Utils exposing (levenshtein)

{-|
TODO: seperate runtimeErrors from compiletime errors
-}
runtimeError : Argument -> Model -> Model
runtimeError = compileError

{-|
add a compile error to the model
-}
compileError : Argument -> Model -> Model
compileError messages model =
  { model | errorMessage <- String.join "\n" <| (model.errorMessage) :: messages }

commandSuggestions : CommandLibrary -> String -> List String
commandSuggestions dict command = 
  if String.trim command == "" then ["no command provided!"]
  else
    List.map fst
    <| List.take 5
    <| List.sortBy (snd) 
    <| List.filter (\(_,d) -> d < 3) 
    <| List.map (\x -> (x, levenshtein command x)) (Dict.keys dict)

{-|
add a command not found error to the error log
-}
commandNotFound : CommandLibrary -> String -> Command
commandNotFound dict command  =
  let
    helpWarning = 
      if (List.length <| String.split " " command) > 2 then "\nMaybe you forgot a $?"
      else 
        let
          suggestions = commandSuggestions dict command
        in
          if List.isEmpty suggestions then "" 
          else "\nMaybe you meant: \n" ++ (String.join "\n" suggestions)
                                        
  in
    CompileError ["command not found: " ++ command ++ helpWarning]