# elm-netlogo

A whitespace-inspired Netlogo clone with functional features.

# Background

If you haven't heard of [Netlogo](https://ccl.northwestern.edu/netlogo/) before, go check it out. It's a Logo derivative aimed at allowing you to create simulations through an agent-based model.

While studying at university, it was probably the best thing they introduced to me and I had a lot of fun abusing it as much as I could, some of which I've documented with these projects - [ask-the-net](https://github.com/eeue56/ask-the-net), [morse-code-golf](https://github.com/eeue56/code-golf/tree/master/morsecode), [zombie-sims](https://github.com/eeue56/NetlogoSims/blob/master/Zombies/ZombiesSim.nlogo).

As an actual language, it doesn't really work very well. It has some fairly limited power, though if you abuse the turtles you can do some powerful things (like in my morse-code example, where I use turtles for implementing dicts).

Whitespace is a fairly nice esoteric language, for which I have an implementation [here](https://github.com/eeue56/spacepie). There were parts of whitespace that I really liked the simplicity of implementing - especially the stack.

elm-netlogo aims to be a simple language, bridging whitespace and Netlogo, just like every programmer has ever wished for.

## Notes

If it hits an error, it will just keep going. It will give you an error message, abort the current line, then try to run the rest of the program.
This can give some interesting if subtle errors.

Empty lines are ignored.

The stack is string-only. In fact, everything is a string, much like tcl. Functions are strings, as are their arguments. Internally things are converted from string at runtime, but everything is a string

## Syntax

`TODO: improve this`

### Comments

Use `;` for comments, eg

```
; hello world
push $ hello world
```

`#` is used to push things off the stack as arguments to the command following

E.g 

```
### set-pcolor-of $ 5, 6 

```

where the stack is

```
["0", "0", "255"]
```

gets turned into 

```
set-pcolor $ 5, 6, 255, 0, 0
```

If you want to pop more items off the stack, use more hashes. It will push an item off the stack for each hash.
If you want to pop everything off the stack, use `#@`. The `@` symbol is used to denote "all".

`>` is the a stack use - the stack remains unchanged after using it, but the effect is the same. This allows you to duplicate items on the stack by doing something like

```
> push
```

`$` is used to seperate commands from their arguments

If a command has no arguments, then it's not needed.

# Example program

Using eval

```
#@ pop
+ $ 1, 2
+ $ 7, 3
## ++
top $ 1
## pcolor-of 
set-pcolor-of $ 5, 5, 255, 0, 0
push $ pcolor-of $ 5, 5
## eval 
push $ set-pcolor-of $ 6, 7

##### eval
```

You can use `apply` to apply an argument to every item on the stack

```
neighbours-of $ 5, 5
patch-at $ 8, 14
apply $ set-pcolor-of $ 255, 5, 5

neighbours-of $ 8, 14
patch-at $ 5, 5
apply $ set-pcolor-of $ 0, 175, 35
```


You can use `filter` to filter the stack using an argument 
The argument should be a function that returns True/False

```

push $ 5, 6, 8
filter $ lt $ 7
; stack will be 5, 7

```



```

push $ 255, 255, 0
push $ 5, 5
>@ set-pcolor-of
; get rid of the coords
## pop
>@ set-pcolor-of $ 2, 2
; empty stack
#@ pop

set-pcolor-of $ 6, 7, 120, 3, 45
pcolor-of $ 6, 7
; add everything on stack together and push it back on stack
#@ add
; use the top item and push it again
> push
> push
#@ set-pcolor-of $ 7, 7

```

This example shows how to use the `>` operator to maintain context for a particular operation.

```
; ensure stack is empty
#@ pop
set-pcolor-of $ 6, 7, 120, 3, 45
pcolor-of $ 6, 7
; add everything on stack together and push it back on stack
#@ add
; use the top item and push it 
repeat $ 2
; 
>>> set-pcolor-of $ 8, 8
>>> set-pcolor-of $ 9, 8
>>> set-pcolor-of $ 10, 8
>>> set-pcolor-of $ 8, 9
>>> set-pcolor-of $ 8, 11
>>> set-pcolor-of $ 8, 12
```

If you want to duplicate something off the stack, use

```
> push
```

If you want to duplicate everything off the stack, use

```
>@ push
```

if you want to clear the stack, use

```
#@ pop
```
