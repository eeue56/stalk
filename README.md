# elm-netlogo


###Rough clone of NetLogo written in Elm.

While working on a proper parser, at the moment the language is stack-based with inspiration from whitespace. 

## Syntax

`\#` is used to push things off the stack as arguments to the command following

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

