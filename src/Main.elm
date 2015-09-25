module Main where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)

import Language.Patches exposing (..)
import Language.Patches.Getters exposing (..)
import Language.Patches.Setters exposing (..)

import Language.Stack exposing (..)
import Language.Maths exposing (..)
import Language.Cmp exposing (..)

import Views exposing (..)

import Parser exposing (..)
import Parser.Errors exposing (compileError, runtimeError)
import Parser.Runner exposing (..)


commands : CommandLibrary
commands = Dict.fromList
  [("clear", always Clear),
   ("still", always Still),
   ("failed", always Failed),
   ("error", CompileError),
   ("eval", Eval),
   ("apply", ApplyRight),
   ("apply-right", ApplyRight),
   ("apply-left", ApplyLeft),
   ("filter", Filter),
   ("reduce", ReduceRight),
   ("reduce-right", ReduceRight),
   ("reduce-left", ReduceLeft),

   -- stack operations
   ("use", UseStack),
   ("drop", DropStack),
   ("push-to", PushToShelfStack),
   ("empty-stack", always EmptyStack),
   ("push", PushToStack),
   ("pushr", ReversePushToStack),
   ("pop", PopOffStack),
   ("repeat", RepeatTopOfStack),
   ("swap", always SwapTopOfStack),
   ("top", BringToTopOfStack),

   -- setters to do with patches
   ("set-pcolor", SetPcolor),
   ("set-pcolor-of", SetPcolorOf),

   -- patch getters
   ("patch-at", PatchAt),
   ("neighbours-of", NeighboursOf),
   ("pcolor-of", PcolorOf),
   ("pxcor-of", PxcorOf),
   ("pycor-of", PycorOf),
   ("pxycor-of", PxycorOf),

   -- patch loggers
   ("log-patch", LogPatch),

   -- math operations
   ("add", Add),
   ("+", Add),
   ("subtract", Subtract),
   ("-", Subtract),
   ("multiply", Multiply),
   ("*", Multiply),
   ("divide", Divide),
   ("/", Divide),
   ("++", Increment),
   ("--", Decrement),

   -- comparison operations
   ("eq", Equals),
   ("==", Equals),
   ("not-eq", NotEquals),
   ("/=", NotEquals),

   ("lt", LessThan),
   ("<", LessThan),
   ("lt-or-eq", LessThanOrEquals),
   ("<=", LessThanOrEquals),

   ("mt", MoreThan),
   (">", MoreThan),
   ("mt-or-eq", MoreThanOrEquals),
   (">=", MoreThanOrEquals),

   ("true", TrueTest),
   ("false", FalseTest)

  ]

update : Action -> Model -> Model
update action model = 
  case action of 
    UpdateText x -> { model | enteredText <- x } 
    Enter -> 
      let
        model' = { model | errorMessage <- "" }
        commands = List.indexedMap (,) <| List.filter (not << String.isEmpty) <| String.lines model'.enteredText 
      in
        List.foldl (\(lineNumber, command) model'' -> runCommand lineNumber (parse command model'') model'') model' commands
    Reset -> runCommand 0 (Clear, 0) model
    Noop -> model

model' : Model
model' = {
  enteredText = "",
  errorMessage = "",
  patches = defaultPatches 25 25,
  commands = commands,
  width = 750,
  height = 750,
  stack = [],
  stackName = "global",
  stackShelf = Dict.empty,
  patchSize =  750 / 25 }

enteredCommands : Signal.Mailbox Action
enteredCommands = Signal.mailbox Noop

model = Signal.foldp
  update
  model'
  enteredCommands.signal

main = Signal.map (view enteredCommands.address) model