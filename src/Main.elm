module Main where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)

import Language.Patches exposing (defaultPatches)
import Parser.Errors exposing (compileError, runtimeError)
import Parser.Runner exposing (programRunner, runCommand)

import Views exposing (..)

{-|
  The command library is where we register Stalk commands
  Sometimes they're just aliases for exisiting commands
  We will allow things to be added to the command library at compile
  and runtime in the future.
-}
commands : CommandLibrary
commands = Dict.fromList
  [("clear", always Clear),
   ("still", always Still),
   ("failed", always Failed),
   ("error", CompileError),

   ("label", Label),
   ("jump", Jump),

   ("eval", Eval),

   ("apply", ApplyRight),
   ("apply-right", ApplyRight),
   ("apply-left", ApplyLeft),

   ("filter", Filter),

   ("reduce", ReduceRight),
   ("reduce-right", ReduceRight),
   ("reduce-left", ReduceLeft),

   -- stack-shelf operations
   ("create-stack", CreateStacks),
   ("use", UseStack),
   ("drop", DropStack),
   ("drop-all", always DropShelf),
   ("push-to", PushToShelfStack),
   ("store-stack", always StoreStack),
   ("empty-stack", always EmptyStack),
   -- current stack operations
   ("push", PushToStack),
   ("pushr", ReversePushToStack),
   ("pop", PopOffStack),
   ("repeat", RepeatTopOfStack),
   ("swap", always SwapTopOfStack),
   ("top", BringToTopOfStack),
   ("reverse", always ReverseStack),

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

   ("spawn", Spawn),
   ("turtle-on", TurtleOn),

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

update : Action -> Program -> Program
update action program =
  case action of
    UpdateText x -> { program | enteredText = x, steps = 0 }
    Enter -> { program | model = programRunner program.enteredText program.model }
    Reset -> { program | model = runCommand 0 (Clear, 0) program.model, steps = 0 }
    Step ->
      case String.split "\n" program.enteredText of
        [] -> program
        x::xs ->
          let
            program' = { program | model = programRunner x program.model }
          in
            { program' | enteredText = String.join "\n" xs, steps = program'.steps + 1 }
    Noop -> program

numberOfPatches = 25
size = 750

model : Model
model = {
  errorMessage = "",
  patches = defaultPatches numberOfPatches numberOfPatches,
  turtles = [],
  commands = commands,
  labels = Dict.empty,
  width = size,
  height = size,
  stack = [],
  stackName = "global",
  stackShelf = Dict.empty,
  patchSize =  size / numberOfPatches }

program' : Program
program' = {
  enteredText = "",
  model = model,
  steps = 0 }


enteredCommands : Signal.Mailbox Action
enteredCommands = Signal.mailbox Noop

program = Signal.foldp
  update
  program'
  enteredCommands.signal

main = Signal.map (view enteredCommands.address) program
