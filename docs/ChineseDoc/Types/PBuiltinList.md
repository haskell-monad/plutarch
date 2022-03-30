# `PBuiltinList`

您将在Plutarch中大量使用内置列表.`PBuiltinList`有一个[`PListLike`](./../Typeclasses/PListLike.md)实例，让您可以从那里访问所有好东西！但是，`PBuiltinList`只能包含内置类型。特别是，它不能包含Plutarch 函数。

您可以使用从`Plutarch.Builtin`导出的`PLift`来表达"仅内置类型"的约束 -

```hs
validBuiltinList :: PLift a => PBuiltinList a
```

如前所述，`PBuiltinList`可以访问所有`PListLike`实用程序。除此之外，`PLift a => PBuiltinList a`还有一个[`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)实例。您可以使用`pcon`构造一个`PBuiltinList`(但您应该更喜欢使用来自`PListLike`的`pcons`):

```hs
> pcon $ PCons (phexByteStr "fe") $ pcon PNil
```

将产生一个带有一个元素的`PBuiltinList PByteString` - `0xfe`。当然，您可以使用`pcons # phexByteStr "fe" # pnil`来代替！

您还可以使用 `pmatch` 来匹配列表:

```hs
pmatch (pcon $ PCons (phexByteStr "fe") $ pcon PNil) $ \case
  PNil -> "hey hey there's nothing here!"
  PCons _ _ -> "oooo fancy!"
```

但是你应该更喜欢 `PListLike` 中的 `pelimList`:

```hs
pelimList (\_ _ -> "oooo fancy") "hey hey there's nothing here!" $ pcon $ PCons (phexByteStr "fe") $ pcon PNil
```

第一个参数是一个在`PCons`情况下调用的函数，列表的头部和尾部作为参数。
第二个参数是列表为空时返回的值。仅当列表为空时才对其进行评估。
最后一个参数是列表本身。

> 旁白: 对`PBuiltinList`(即Plutus Core内置列表)的底层细节感兴趣？您可以在[Plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md)找到所有您需要了解的信息。