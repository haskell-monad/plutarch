# `PString`

`Term s PString` 有一个 `IsString`实例。这允许您从常规字符串字面量创建Plutarch级别的字符串terms，前提是您打开了`OverloadedStrings`。

```haskell
{-# LANGUAGE OverloadedStrings #-}

"foo"
```

其中`foo`实际上是`Term s PString`。

它还有一个`PEq`实例。它的terms有`Semigroup`和`Monoid`实例 —— 它们的工作方式与你期望的一样。

它没有`PlutusType`实例.

这是Plutus Core[内置字符串](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinString)(实际上是`Text`)的同义词。