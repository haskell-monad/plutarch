# `PIsData`

`PIsData`类型类有助于在Plutarch类型及其对应的[`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)表示之间进行简单且类型安全的转换. 它通过 [`PAsData`](./../Types/PAsData.md)跟踪类型信息。


```hs
class PIsData a where
  pfromData :: Term s (PAsData a) -> Term s a
  pdata :: Term s a -> Term s (PAsData a)
```

[`PInteger`](./../Types/PInteger.md)有一个`PIsData`实例。[`PInteger`](./../Types/PInteger.md)有一个`PIsData`实例。当然, `PInteger`的`PData`表示是`I`数据。您可以使用`UnIData`(即`pasInt`)从`I`数据中获取`PInteger`。

```hs
instance PIsData PInteger where
  pfromData x = pasInt # pforgetData x
  pdata x = punsafeBuiltin PLC.IData # x
```

本质上，`pdata`将`PInteger`包装成`I`数据值。而`pfromData`只是简单地解开`I`数据值以获取`PInteger`。

> 旁白: 您可能会问，什么是`I`数据值？这是指`Data`/`BuiltinData`的不同构造函数。您可以在[Plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)找到对此的完整解释。

对于仅将内置类型包装到`Data`中的简单构造函数，例如integers、bytestrings、lists和map，`PIsData`的工作方式与上面大致相同。但是，`Constr` 数据值呢？当您的ADT不直接对应于那些简单的内置类型时 - 但您仍然需要将其编码为 `Data`(例如`PScriptContext`)。在这种情况下，您应该[实现`PIsDataRepr`](./PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)，您将免费获得 `PIsData`实例！