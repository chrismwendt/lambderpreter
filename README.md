# lambderpreter

This is an interpreter for the lambda calculus.

## Plans

- runtime system
- hybrid strict/lazy
- mixfix syntax
- dependent types
- overloaded names
- metaprogramming via quoting

## Examples

Identity applied to identity is identity:

```
> (a -> a) (b -> b)
(b -> b)
```

False and true is false:

```
> (T -> (F -> (NOT -> (AND -> (OR -> AND F T))) (b -> b F T) (l -> (r -> l r F)) (l -> (r -> l T r)))) (a -> (b -> a)) (a -> (b -> b))
(a -> (b -> b))
```

The factorial of 3 is 6:

```
> (Y -> (zero -> (inc -> (add -> (mul -> (fac -> fac (inc (inc (inc zero)))) (Y (fac -> (n -> n (inc zero) (k -> mul n (fac k)))))) (Y (mul -> (n -> (m -> m zero (k -> add n (mul n k))))))) (Y (add -> (n -> (m -> m n (k -> inc (add n k)))))))) (z -> (s -> z)) (n -> (z -> (s -> s n)))) (f -> (a -> f (a a)) (a -> f (a a)))
(z -> (s -> (s (z -> (s -> (s (z -> (s -> (s (z -> (s -> (s (z -> (s -> (s (z -> (s -> (s (z -> (s -> z))))))))))))))))))))
```
