# Prefer pattern matching on the result of `pmatch` immediately
立即优先对 `pmatch` 的结果进行模式匹配

您应该立即尝试对`pmatch`的结果进行模式匹配。这是因为`pmatch`的语义将使您在模式匹配之前编写的任何内容都内联到每个分支:

```hs
this :: Term s (PScriptPurpose :--> PInteger)
this = plam $ \x -> pmatch x $ \l ->
  plet $ 1 + 2 $ \i -> case l of
    PMinting _ -> i + 3
    PSpending _ -> i + 4
    PRewarding _ -> i + 5
    PCertifying _ -> i + 6
```

请注意，上面的代码`plet`是如何在匹配`l`即`pmatch`结果之前进行计算的。这将使`plet $ 1 + 2 $ \i -> i + <something>`内联到模式匹配的每个分支中！也就是说，它不仅每次都会计算`1 + 2`，还会`plet`它，这引入了一个额外的lambda，只是为了立即应用 lambda！

只要有可能，您应该始终立即匹配结果:

```hs
this :: Term s (PScriptPurpose :--> PInteger)
this = plam $ \x -> plet $ 1 + 2 $ \i ->
  pmatch x $ \case
    PMinting _ -> i + 3
    PSpending _ -> i + 4
    PRewarding _ -> i + 5
    PCertifying _ -> i + 6
```

这适用于`do`语法(无论是使用`TermCont`还是使用`QualifiedDo`)。尝试使用内联部分模式匹配(例如 `PMinting _ <- pmatch x`)，或在下一行使用模式匹配(例如 `l <- pmatch x; case l of ...`)。