# Recursion

要在[UPLC (Untyped Plutus Core)](https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md)中模拟递归，您需要使用[Y组合器](https://stackoverflow.com/questions/93526/what-is-ay-combinator)。Plutarch提供的Y组合器的名称为`pfix`:

```haskell
pfix :: Term s (((a :--> b) :--> (a :--> b)) :--> (a :--> b))
```

尽管类型很吓人，但它可以按您的预期工作。将其视为Haskell类型:

```haskell
fix :: ((a -> b) -> (a -> b)) -> (a -> b)
```

第一个参数是"self"，或者是你想要递归的函数。

下面的示例实现了Plutarch级别的阶乘函数:

```haskell
import Plutarch.Prelude

pfac :: Term s (PInteger :--> PInteger)
pfac = pfix #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)
-- (ignore the existence of non positives :D)
```

注意`f`是如何接受`self`并在其上递归的。您所要做的就是通过在`f`上使用`plam`创建一个Plutarch级别的函数，并`pfix`结果 - `self`参数将为您处理。