# `PAsData`

这是一种表示[`BuiltinData`/`Data`]的类型化方式(https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). 强烈建议您使用`PAsData`来跟踪您实际拥有的`Data`值的"species"(种类)。`Data`可以是`Constr`(用于products总和 - ADT)、`Map`(用于将Data的关联映射包装到Data)、`List`(用于包装内置数据列表)、`I`(用于包装内置整数)和`B`(用于包装内置bytestrings)。

考虑一个函数，它接收并返回一个`B`数据值 - 也就是`ByteString`作为`Data`值。如果您使用直接Plutarch的同义词来表示`Data` - `PData`，您将拥有:

```hs
foo :: Term s (PData :--> PData)
```

这并不是很有用 —— 你无法确保你实际上正在使用的是`B`数据值。你可以用`PAsData`来代替:

```hs
foo :: Term s (PAsData PByteString :--> PAsData PByteString)
```

现在，您可以确定您正在使用一个实际代表内置字节串的`Data`值！

[`PIsData`](./../Typeclasses/PIsData.md)类型类提供对`PAsData` terms的包装和展开。具体来说，通过函数 - `pfromData` 和 `pdata`。

这些函数的一些有用实例:

```hs
pfromData :: Term s (PAsData PInteger) -> Term s PInteger

pfromData :: Term s (PAsData PByteString) -> Term s PByteString

pfromData :: Term s (PAsData (PBuiltinList (PAsData a))) -> Term s (PBuiltinList (PAsData a))

pdata :: Term s PInteger -> Term s (PAsData PInteger)

pdata :: Term s PByteString -> Term s (PAsData PByteString)

pdata :: Term s (PBuiltinList (PAsData a)) -> Term s (PAsData (PBuiltinList (PAsData a)))
```
