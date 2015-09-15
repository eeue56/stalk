# elm-netlogo

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

The stack is string-only

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

If you want to pop more items off the stack, use more hashes.
If you want to pop everything off the stack, use `#@`


`$` is used to seperate commands from their arguments

# Example program

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