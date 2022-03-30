# 弄清楚Plutarch类型的表示

我们之前讨论过[Plutarch类型只是标签](./../Introduction/Plutarch%20Types.md)并且与它们的运行时表示没有直接联系。不过，能够从数据类型声明中直观地找出运行时表示是很重要的。这就是大多数类型遵循某些约定的原因。

表示只能是两个类别之一: 内置和Scott编码。 Plutarch中已经定义了所有`trivial`(普通的)内置类型：`PInteger`、`PByteString`、`PString`、`PBool`、`PUnit`、`PBuiltinList` 和 `PBuiltinPair`。

现在，让我们讨论数据声明的模式以及它们应该暗示的表示形式:

- 如果它是包含`Plutarch`类型的`term`的`newtype` - 它应该具有与底层Plutarch类型相同的表示.

  例如: `newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)`只是表示为`PByteString`. 这是通过使用[`DerivePNewtype`](./../Usage/Deriving%20for%20newtypes.md)派生所有必要的实例(特别是`PlutusType`)来确保的。

- If it's an ADT that derives `PlutusType` generically (i.e. `derive anyclass (PlutusType)`)- it uses Scott encoding. This is typically the encoding you want for non-trivial data types that don't need to be part of datums or redeemers.
- 如果它是一般派生`PlutusType`的`ADT`(即:`derive anyclass (PlutusType)`)，它使用Scott编码。这通常是您想要的`non-trivial`(有意义的)数据类型的编码, 这些数据类型不需要作为`datums`或`redeemers`的一部分。

  e.g. `PList` derives `PlutusType` generically and is represented with Scott encoding. 
  例如`PList`一般派生`PlutusType`并用Scott编码表示。

- If it's an ADT that derives `PIsDataRepr` generically (i.e. `derive anyclass (PIsDataRepr)`), as well as `PlutusType` via `PIsDataReprInstances`, it's data encoded. Particularly, it's a [`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) value - which is part of the builtin types.
- 如果它是一般派生`PIsDataRepr`的`ADT`(即:`derive anyclass (PIsDataRepr)`), 以及通过`PIsDataReprInstances`的`PlutusType`，它是`data`编码的. 特别是，它是一个[`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data)值 - 它是内置类型的一部分。

  e.g. `PScriptContext` derives `PIsDataRepr` generically and `PlutusType` via `PIsDataReprInstances`.
  例如: `PScriptContext`一般派生 `PIsDataRepr`并通过`PIsDataReprInstances`派生`PlutusType`。
