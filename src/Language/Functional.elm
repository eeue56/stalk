module Language.Functional where

import String
import Debug exposing (log)
import List.Extra

import Model exposing (..)
import Language.Stack as Stack

import Utils

import Parser
import Parser.Models exposing (Runner)
import Parser.Symbols exposing (..)
import Parser.Errors exposing (..)

{-
  When no args, there should be a dollar, empty list
  When the args is just the function name (len args == 1) then it should the func name + dollar, empty list
  Otherwise, there should be an empty string, the args

  This assumes that more than one args means that a paritial function has been provided,
  meaning that there's no need for a dollar
  -}
functionPartition : Argument -> (String, Argument)
functionPartition args =
  case args of
    [] -> (funcSplitter, [])
    -- when there's only one argument, we need to check if there's
    -- already a dollar involved
    x::[] ->
      case String.split funcSplitter x of
        [] -> (funcSplitter, [])
        y::[] -> (x ++ (" " ++ funcSplitter ++ " "), [])
        y::ys -> (y ++ funcSplitter, ys)
    x::xs -> ("", args)

-- apply's args should be a partial function taking one arg which pushes something to the stack
apply : Bool -> Runner -> Argument -> Model -> Model
apply folder runner args model =
  let
    folder' =  if folder then List.foldr else List.foldl
    command extraArgs =
      let
        (func, args') = log "partition" <| functionPartition args
      in
        Parser.parse (func ++ (String.join " , " <| args' ++ [extraArgs])) model
  in
    folder' (\extra model -> runner 0 (log "extra: " <| command extra) model) (Stack.emptyStack model) model.stack

applyRight : Runner -> Argument -> Model -> Model
applyRight = apply True

-- TODO:
-- identify WTF bug that happens if set applyLeft to partially applied func
applyLeft : Runner -> Argument -> Model -> Model
applyLeft runner args model = apply False runner args model


-- filter's args should be a partial function taking one arg which pushes a bool onto the stack
filter : Runner -> Argument -> Model -> Model
filter runner args model =
  let
    appliedModel = applyRight runner args model
    passed zipped = List.map (fst) <| List.filter (\(x, val) -> val == "True") zipped
  in
    case List.length (model.stack) == List.length appliedModel.stack of
      True ->
        case List.map2 (,) model.stack appliedModel.stack of
          [] -> model
          xs -> { model | stack = passed xs }
      False ->
        runtimeError (["For some reason apply made a different sized stack, model stack:"] ++ model.stack ++ ["\napplied stack:"] ++ appliedModel.stack) model


-- reduce's args should be a function that takes two args and pushed one back on the stack
-- TODO: implement
reduce : Bool -> Runner -> Argument -> Model -> Model
reduce folder runner args model =
  let
    folder' = if folder then List.foldr else List.foldl
    (n, args') =
      case args of
        [] -> (0, [])
        x::xs ->
          case String.toInt x of
            Err _ -> (List.length model.stack, args)
            Ok v -> (v, xs)

    command extraArgs =
      let
        (func, args'') = log "partition" <| functionPartition args'
      in
        Parser.parse (func ++ (String.join " , " <| args'' ++ extraArgs)) model
  in
    case Utils.takeN n <| List.reverse <| model.stack of
      Nothing -> runtimeError ["Uneven stack!"] model
      Just groupedArgs ->
        folder' (\extra model -> runner 0 (log "extra: " <| command extra) model) (Stack.emptyStack model) groupedArgs

reduceRight : Runner -> Argument -> Model -> Model
reduceRight = reduce True

reduceLeft : Runner -> Argument -> Model -> Model
reduceLeft = reduce False


takeWhile : Runner -> Argument -> Model -> Model
takeWhile runner args model =
  let
    appliedModel = applyRight runner args model
    passed zipped = List.map (fst) <| List.Extra.takeWhile (\(x, val) -> val == "True") zipped
  in
    case List.length (model.stack) == List.length appliedModel.stack of
      True ->
        case List.map2 (,) model.stack appliedModel.stack of
          [] -> model
          xs -> { model | stack = passed xs }
      False ->
        runtimeError (["For some reason apply made a different sized stack, model stack:"] ++ model.stack ++ ["\napplied stack:"] ++ appliedModel.stack) model

dropWhile : Runner -> Argument -> Model -> Model
dropWhile runner args model =
  let
    appliedModel = applyRight runner args model
    passed zipped = List.map (fst) <| List.Extra.dropWhile (\(x, val) -> val == "True") zipped
  in
    case List.length (model.stack) == List.length appliedModel.stack of
      True ->
        case List.map2 (,) model.stack appliedModel.stack of
          [] -> model
          xs -> { model | stack = passed xs }
      False ->
        runtimeError (["For some reason apply made a different sized stack, model stack:"] ++ model.stack ++ ["\napplied stack:"] ++ appliedModel.stack) model
