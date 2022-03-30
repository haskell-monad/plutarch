# `PTuple`

这些是[data编码](./../Concepts/Data%20and%20Scott%20encoding.md#data-encoding)元组对。您可以使用`ptuple`构建`PTuple`:

```hs
ptuple :: Term s (PAsData a :--> PAsData b :--> PTuple a b)
```

`PTuple`有一个[`PDataFields`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#all-about-extracting-fields)实例。因此，您可以使用`pletFields`或`pfield`提取其字段。

由于`PAsData (PBuiltinPair (PAsData a) (PAsData b))`和`PAsData (PTuple a b)`具有相同的表示 - 您可以安全地在它们之间进行转换, 而无需任何成本:

```hs
ptupleFromBuiltin :: Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b))) -> Term s (PAsData (PTuple a b))

pbuiltinPairFromTuple :: Term s (PAsData (PTuple a b)) -> Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b)))
```
