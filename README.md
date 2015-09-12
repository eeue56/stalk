# elm-netlogo


###Rough clone of NetLogo written in Elm.

While working on a proper parser, at the moment the language is stack-based with inspiration from whitespace. 

## Syntax

`#` is used to push things off the stack as arguments to the command following

E.g 

```
# set-pcolor-of $ 5, 6 

```

where the stack is

```
["255, 0, 0"]
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
; add everything on stack together and pop it back on stack
#@ add
; use the top item and push it again
> push
> push
#@ set-pcolor-of $ 7, 7

```