# Strictness and Laziness; Delayed Terms and Forcing

Plutarch和UPLC一样，默认是严格的；这与非严格的Haskell形成鲜明对比。在实践中，这意味着在Plutarch中调用函数会预先评估Plutarch lambda `Term`的所有参数。

> 注意：下面的例子并不完全对应`pif` 或 `pif'`的实现；仅用于教学目的

例如，当两个`Term`中的一个在`if`语句中被分支时，这种行为可能是不可取的。Plutarch级别的函数`pif'`的参数自然是严格的 - 因此在进入函数体之前对两个分支进行求值。

```hs
pif' :: Term s (PBool :--> b :--> b :--> b)
pif' = plam hif
```

一个严格的`if`是不可取的，原因很明显: 我们在运行时只遵循一个分支，因此在检查我们应用`pif`的`PBool`值之前评估这两个分支是没有意义的。

为了避免这种情况，我们使用`pdelay`来创建一个"延迟的`Term`"。`pdelay`包装term的`PType`标记，覆盖默认的严格行为并指示不应立即评估该term。`pdelay`具有以下类型:

```hs
pdelay :: Term s a -> Term s (PDelayed a)
```

延迟的term在使用`pforce`函数强制执行时进行评估。强制term剥离`PDelayed`包装器:

```hs
pforce :: Term s (PDelayed a) -> Term s a
```

因此，如果我们想要一个惰性`pif`，我们可以执行以下操作:

```hs
-- | 使用带有`pdelay`和`pforce`的Haskell级别函数来对`pif`进行惰性包装。
hif :: Term s PBool -> Term s a -> Term s a -> Term s a
hif cond whenTrue whenFalse = pforce $ pif' # cond # pdelay whenTrue # pdelay whenFalse
```

注意事项: 在同一延迟term上调用`pforce`两次将每次都执行计算。熟悉Haskell处理惰性的用户 —— 强制thunk两次永远不会重复计算 —— 应该注意UPLC的行为不同。

最后，读者应该注意，在编写Plutarch脚本时，`pdelay` 和 `pforce`是非常强大的工具，因此鼓励读者熟悉它们。