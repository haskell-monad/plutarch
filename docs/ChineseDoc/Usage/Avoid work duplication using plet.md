# `plet` to avoid work duplication

有时，在使用Plutarch terms编写Haskell级别的函数时，您可能会发现自己需要多次重用Haskell级别函数的参数:

```hs
foo :: Term s PString -> Term s PString
foo x = x <> x
```

在这种情况下，您应该在参数上使用`plet`来[避免重复工作](./../Tricks/Don't%20duplicate%20work.md)。