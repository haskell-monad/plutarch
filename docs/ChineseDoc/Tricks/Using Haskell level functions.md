# When to use Haskell level functions?
# 何时使用Haskell级别的函数？

虽然您通常应该[更喜欢Plutarch级别的函数](./Prefer%20Plutarch%20functions.md)，但有时Haskell级别的函数实际上要好得多。然而，弄清楚何时是这种情况 - 是一门微妙的艺术。

不过，当您希望对函数参数进行惰性计算时，有一个简单明了的用例。在这种情况下，您应该在调用某些Plutarch级函数之前使用以`pdelay`为参数的Haskell级函数. 回想一下[Plutarch级别的函数是严格的](./Plutarch%20functions%20strict.md)。

除了这个简单的用例之外，弄清楚何时使用Haskell级别的函数是相当复杂的。在生成Plutus Core时，Haskell级别的函数将始终被内联。除非该函数只使用一次，否则这种内联会增加脚本的大小 —— 这是有问题的。

但是，如果该函数只使用一次，并且将其设为Plutarch级别会导致引入额外的`plam`和`#` - 那么您应该将其设为Haskell级别。例如，考虑`pelimList`实现：

```hs
pelimList :: PLift a => Term s (a :--> PBuiltinList a :--> r) -> Term s r -> Term s (PBuiltinList a) -> Term s r
pelimList match_cons match_nil ls = pmatch ls $ \case
  PCons x xs -> match_cons # x # xs
  PNil -> match_nil
```

它接受了一个Plutarch级别的函数，我们来看一个典型的用法:

```hs
pelimList
  (plam $ \x xs -> pcons # x # (self # xs))
  pnil
  ls
```

这是相当多余的，上面的代码片段将转换为:

```hs
pmatch ls $ \case
  PCons x xs -> (plam $ \x xs -> pcons # x # (self # xs)) # x # xs
  PNil -> match_nil
```

引入了额外的`plam`和`#`。实际上，`pelimList`可以采用Haskell级别的函数:

```hs
pelimList :: PLift a => (Term s a -> Term s (PBuiltinList a) :--> Term s r) -> Term s r -> Term s (PBuiltinList a) -> Term s r
pelimList match_cons match_nil ls = pmatch ls $ \case
  PCons x xs -> match_cons x xs
  PNil -> match_nil
```

现在，以下用法:

```hs
pelimList
  (\x xs -> pcons # x # (self # xs))
  pnil
  ls
```

会变成:

```hs
pmatch ls $ \case
  PCons x xs -> pcons # x # (self # xs)
  PNil -> match_nil
```

事实证明，使用`pelimList`几乎总是涉及使用一次性的Haskell级别函数(因此是冗余的`plam`)。因此，`pelimList`受益于直接采用Haskell级别的函数。

然而，并不是所有的高阶函数都能从Haskell级函数中受益。在许多高阶函数用例中，传递一个常用的函数参数，而不是一次性的函数参数，会让您受益匪浅。想象一下`map`，你并不总是使用一次性函数进行映射 —— 通常，你使用现有的常用函数进行`map`。在这些情况下，那个常用的函数应该是Plutarch级别的函数，因此它可以被`hoisted`(提升)并且`map`可以简单地引用它。
