module Parser.Runner where

import Model exposing (..)

import Language.Patches exposing (..)
import Language.Patches.Getters exposing (..)
import Language.Patches.Setters exposing (..)

import Language.Stack as Stack
import Language.Stack exposing (..)
import Language.Maths exposing (..)
import Language.Cmp exposing (..)

import String
import Debug exposing (log)
import Utils

import Parser exposing (..)
import Parser.Errors exposing (compileError, runtimeError)
import Parser.Symbols exposing (funcSplitter)


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
        y::[] -> (x ++ (" " ++ funcSplitter ++ " "), [])
        y::ys -> (y ++ funcSplitter, ys)
    x::xs -> ("", args)

-- apply's args should be a partial function taking one arg which pushes something to the stack
apply : Bool ->  Argument -> Model -> Model
apply folder args model = 
  let 
    folder' =  if folder then List.foldr else List.foldl 
    command extraArgs = 
      let
        (func, args') = log "partition" <| functionPartition args
      in
        Parser.parse (func ++ (String.join " , " <| args' ++ [extraArgs])) model
  in
    folder' (\extra model -> runCommand 0 (log "extra: " <| command extra) model) (Stack.emptyStack model) model.stack

applyRight : Argument -> Model -> Model
applyRight = apply True

-- TODO:
-- identify WTF bug that happens if set applyLeft to partially applied func
applyLeft : Argument -> Model -> Model 
applyLeft args model = apply False args model


-- filter's args should be a partial function taking one arg which pushes a bool onto the stack
filter : Argument -> Model -> Model 
filter args model = 
  let
    appliedModel = applyRight args model 
    passed zipped = List.map (fst) <| List.filter (\(x, val) -> val == "True") zipped
  in
    case List.length (model.stack) == List.length appliedModel.stack of
      True -> 
        case List.map2 (,) model.stack appliedModel.stack of 
          xs -> { model | stack <- passed xs }
          [] -> model
      False -> 
        runtimeError (["For some reason apply made a different sized stack, model stack:"] ++ model.stack ++ ["\napplied stack:"] ++ appliedModel.stack) model 

-- reduce's args should be a function that takes two args and pushed one back on the stack
-- TODO: implement
reduce : Bool -> Argument -> Model -> Model
reduce folder args model = 
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
        folder' (\extra model -> runCommand 0 (log "extra: " <| command extra) model) (Stack.emptyStack model) groupedArgs

reduceRight : Argument -> Model -> Model
reduceRight = reduce (True)

reduceLeft : Argument -> Model -> Model
reduceLeft = reduce (False)

runCommand : Int -> (Command, Int) -> Model -> Model
runCommand lineNumber (command, stackUses) model' =
  let
    model = { model' | stack <- List.drop stackUses model'.stack }
  in
    case command of
      Eval args -> runCommand lineNumber (Parser.parse (String.join "," args) model) model
      ApplyRight args -> applyRight args model 
      ApplyLeft args -> applyLeft args model
      Filter args -> filter args model
      ReduceRight args -> reduceRight args model
      ReduceLeft args -> reduceLeft args model

      SetPcolor color -> setPcolor color model
      SetPcolorOf args -> setPcolorOf args model

      PatchAt args -> patchAt args model
      NeighboursOf args -> neighboursOf args model
      PcolorOf args -> pcolorOf args model
      PxcorOf args -> pxcorOf args model
      PycorOf args -> pycorOf args model
      PxycorOf args -> pxycorOf args model

      LogPatch coors -> logPatch coors model

      UseStack args -> Stack.use args model
      DropStack args -> Stack.drop args model
      PushToShelfStack args -> Stack.pushToShelfStack args model
      StoreStack -> Stack.storeCurrentStack model
      EmptyStack -> emptyStack model
      DropShelf -> Stack.dropAllStacks model

      PushToStack args -> pushToStack args model
      ReversePushToStack args -> pushReverseToStack args model
      PopOffStack args -> popOffStack args model
      RepeatTopOfStack args -> repeatTopOfStack args model
      SwapTopOfStack -> Stack.swap model
      BringToTopOfStack args -> bringToTopOfStack args model

      Add args -> add args model
      Subtract args -> subtract args model
      Multiply args -> multiply args model
      Divide args -> divide args model
      Increment args -> increment args model
      Decrement args -> decrement args model

      Equals args -> eq args model
      NotEquals args -> notEq args model
      MoreThan args -> moreThan args model
      MoreThanOrEquals args -> moreThanOrEquals args model
      LessThan args -> lessThan args model
      LessThanOrEquals args -> lessThanOrEquals args model
      TrueTest args -> true args model
      FalseTest args -> false args model

      CompileError messages -> compileError (["Error on line: " ++ toString lineNumber] ++ messages) model
      Clear -> clearPatches model |> emptyStack |> dropAllStacks
      Still -> model
      Failed -> runtimeError ["Failed."] model
      _ -> compileError ["Command type not found"] model
    
programRunner : String -> Model -> Model
programRunner enteredText model =      
  let
    model' = { model | errorMessage <- "" }
    commands = List.indexedMap (,) <| List.filter (not << String.isEmpty) <| String.lines enteredText 
  in
    List.foldl (\(lineNumber, command) model'' -> runCommand lineNumber (parse command model'') model'') model' commands
 