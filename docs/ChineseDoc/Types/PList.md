# `PList`


这是`PBuiltinList`的[Scott编码](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding)表亲。这是什么意思？好吧，在实践中，它只是意味着`PList`可以包含任意term - 而不仅仅是内置类型。`PList`也有一个[`PListLike`](./../Typeclasses/PListLike.md)实例 - 所以你不会错过这里的任何这些实用程序！

`PList`也有一个[`PlutusType`](../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)实例。您可以使用`pcon`构造一个`PList`(但您应该更喜欢使用来自`PListLike`的`pcons`):

```hs
> pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
```

将产生一个带有一个元素的`PList PByteString` - `0xfe`。当然，您可以使用`pcons # phexByteStr "fe" # pnil`来代替！

您还可以使用`pmatch`来匹配列表:

```hs
pmatch (pcon $ PSCons (phexByteStr "fe") $ pcon PSNil) $ \case
  PSNil -> "hey hey there's nothing here!"
  PSCons _ _ -> "oooo fancy!"
```

但是你应该更喜欢`PListLike`中的`pelimList`:

```hs
pelimList (\_ _ -> "oooo fancy") "hey hey there's nothing here!" $ pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
```
