# `PBool`

Plutarch级别的布尔terms可以使用`pconstant True`和`pconstant False`构造。

```haskell
pif (pconstant PFalse) 7 42
-- evaluates to 42
```

您可以使用`#&&` 和 `#||`组合Plutarch布尔terms，它们是 `&&` 和 `||` 的同义词。这些是Haskell级别的运算符，因此具有短路功能。如果您不需要短路，您可以分别使用Plutarch级别的替代方案 - `pand'` 和 `por'`。

这是Plutus Core[builtin boolean](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinBool)的同义词。