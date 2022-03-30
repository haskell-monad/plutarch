# `PBuiltinPair`

就像内置列表的情况一样，您将在这里使用内置函数（或者更确切地说，Plutarch内置函数的同义词）。你可以在[builtin-pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md)中找到所有相关信息。请随意阅读`Plutarch`示例。

特别是，您可以使用`pfstBuiltin`和`psndBuiltin`解构`PBuiltinPair`。您可以使用`ppairDataBuiltin`构建`PBuiltinPair (PAsData a) (PAsData b)` terms:

```hs
ppairDataBuiltin :: Term s (PAsData a :--> PAsData b :--> PBuiltinPair (PAsData a) (PAsData b))
```

注意`PAsData (PBuiltinPair (PAsData a) (PAsData b))`和`PAsData (PTuple a b)`实际上在底层具有相同的表示这也很有帮助。参见[`PTuple`](./PTuple.md)