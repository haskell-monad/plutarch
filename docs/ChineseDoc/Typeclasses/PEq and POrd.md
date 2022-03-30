# `PEq` & `POrd`

`PEq`类型类提供了Plutarch级别的相等性:

```haskell
class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool
```

`PInteger`实现了`PEq`，正如你所期望的那样。所以你可以这样做:

```haskell
1 #== 2
```

这将产生一个 `Term s PBool`，你可能会将其与`pif`(或类似的)一起使用.

同样，`POrd`模拟`Ord`:

```haskell
class POrd t where
  (#<=) :: Term s t -> Term s t -> Term s PBool
  (#<) :: Term s t -> Term s t -> Term s PBool
```

它可以按您的预期工作:

```haskell
{-# LANGUAGE OverloadedStrings #-}

pif (1 #< 7) "indeed" "what"
```

评估为`"indeed"` - 类型为`Term s PString`。