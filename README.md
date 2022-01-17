## Denotational Semantics Interpreter

A denotational semantics intepreter written in Haskell for an dynamically typed language. There are three supported types of evaluated expressions: `Integer`, `Boolean`, `Cons cell`. 

### Syntax
```haskell
-- Commands
data C = Cskip 
        | Cassign Var E 
        | Cseq C C 
        | Cfor E C 
        | Cif E C C 
        | Cwhile E C
        deriving Show

-- Expressions
data E = Ezero 
        | Esucc E 
        | Epred E 
        | Eif E E E 
        | Evar Var
        | Etrue 
        | Efalse 
        | Elt E E 
        | Eeq E E 
        | Enot E
        | Econs E E 
        | Ehd E 
        | Etl E
        deriving Show
```

### Examples

```bash
$ cat ex1
result := 0;
for succ succ succ succ succ succ 0 do
for succ succ succ succ succ succ succ 0 do
result := succ result
```

```
$ runhaskell WhileSem.hs < testcases/test1
42
```

```bash
$ cat ex2
x := succ succ succ succ succ succ succ 0;
result := false;
while 0 < x do (
result := x : result;
x := pred x
)
```

```
$ runhaskell WhileSem.hs < testcases/test2
1 : 2 : 3 : 4 : 5 : 6 : 7 : false
```
