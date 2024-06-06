# Obelus

Obelus is a dependently typed ml. 
This allows the syntax of the language to be a single tree, as types, modules, and terms all use the same syntax.
Additionally, the syntax is a modified version of m-exprs with a curried application style.
There are three forms:

* Atoms: variables, numbers, strings, etc.
* Compounds: function/constructor/infix application e.g. `f x y + g z` (the corresponding m-expr is `+[f [x;y];g [z]]`)
    + lambdas are a special operator `->` which allows multiple parameters at once `x y z -> x` is `x -> y -> z -> x`
    + Compounds are grouped with `()` and there is otherwise no precedence between operators
* Blocks: modules, records, tuples, and lists `[let a:A = x; let b:B a = f a; let c:C a b = g a b]`, `[a=x;b=y;c=z]`, or `[x;y;z]`.
    + `let` is only required to ascribe a field a type
    + Blocks support guards, case expressions, and lambdas defined by patterns `let not = [True -> False; False -> True]`
    + Blocks allow both `[]` and `{}`

