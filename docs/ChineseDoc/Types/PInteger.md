# `PInteger`

`Term s PInteger`有一个方便的`Num`实例，允许您从整数字面量构造Plutarch级别的整数terms。这也意味着您可以使用所有典型的算术运算:

```haskell
1 + 2
```

其中`1`和`2`是`Term s PInteger`s。

除了`Num`，它还有一个`PIntegral` 实例，允许您使用除法、模数等。

它还有一个`PEq`和`POrd`实例，允许您进行Plutarch级别的相等和比较。

它没有`PlutusType`实例。

这是Plutus Core[内置整数](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:Integer)的同义词。