# Optimizing unhoistable lambdas

通常，您将在使用自由变量的Plutarch级别函数中创建`utility`(实用)函数。在这种情况下，该函数是不可提升的(即，您不能在其上使用`phoistAcyclic`)。但是，您的目标很可能是在您的主要Plutarch级别函数中多次使用此`utility`(实用程序)函数。此时，您的unhoisted函数将在您每次使用时内联，因此会增加脚本大小。


```hs
pfoo :: Term s (PInteger :--> PBuiltinList PInteger :--> PInteger)
pfoo = plam $ \x l ->
  let innerf = plam $ \y -> x + y
  in innerf # 42 + plength # (pmap # innerf # l)
```

在这里，`innerf`的两种用法都将内联lambda然后应用。这是有问题的，因为您可能想要一个可以简单地用变量引用的lambda。

在这些情况下，您可以简单地[使用`plet`](./Don't%20duplicate%20work.md)就像[在其他地方](../Usage/Avoid%20work%20duplication%20using%20plet.md)

```hs
pfoo :: Term s (PInteger :--> PBuiltinList PInteger :--> PInteger)
pfoo = plam $ \x l ->
  plet (plam $ \y -> x + y) $ \innerf ->
    innerf # 42 + plength # (pmap # innerf # l)
```
