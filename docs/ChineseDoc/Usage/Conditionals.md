# Conditionals

您可以使用`pif`在Plutarch级别模拟`if/then/else`:

```haskell
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
```

这与 Haskell 的 `if/then/else` 具有相似的语义。也就是说，只有谓词持有的分支 - 被评估。

```haskell
pif (pconstant True) 1 2
```

上面的计算结果为`1`，其类型为`Term s PInteger`.