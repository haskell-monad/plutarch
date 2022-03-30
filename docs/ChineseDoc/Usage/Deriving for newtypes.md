# Deriving typeclasses for `newtype`s

如果您要为现有的Plutarch类型定义一个`newtype`，如下所示:

```hs
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
```

理想情况下，您希望只是将这个`newtype`表示为引擎盖下的`PByteString`。因此，`PByteString`的所有类型类实例对于 `PPubKeyHash`也是有意义的。在这种情况下，您也可以简单地为您的`PPubKeyHash`类型派生所有这些类型类! 通过`DerivePNewtype`:

```hs
{-# LANGUAGE UndecidableInstances #-}

import Plutarch.Prelude

newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKeyHash PByteString)
```

(是的，你需要`UndecidableInstances`来派生`PlutusType`)

`DerivePNewtype`接受两个类型参数。它们都是Plutarch类型(i.e. types with kind `PType`即具有`PType`种类的类型)。第一个是您要为其派生实例的类型，而第二个是内部类型(无论`PPubKeyHash`是`newtype`)。

> 注意：重要的是要注意，旨在成为Plutarch类型(即可以表示为Plutarch term)的`newtype`的内容也必须是Plutarch term。`PByteString s`类型在编译后根本不存在于Plutus Core世界中。这一切都只是`Term`s。因此，当您说`Term s PPubKeyHash`时，实际上只是在底层描述了`Term s PByteString` - 因为它在运行时就是这样。

> 另外: 您可以使用`pto`访问内部类型(假设它是`PlutusType`实例).例如，`pto x`，其中`x :: Term s PPubKeyHash`，会给你`Term s PByteString`。`pto`将[`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)term转换为其内部类型。这非常有用，例如，当您需要使用对`bytestring term`进行操作的函数时，您所拥有的只是一个`Term s PPubKeyHash`。你知道它实际上是底层的一个`bytestring` —— 但是你如何获得它呢？使用`pto`!

目前，`DerivePNewtype`允许您为您的Plutarch types派生以下类型类:

- `PlutusType`
- `PIsData`
- `PEq`
- `POrd`
- `PIntegral`

您还可以为Plutarch terms 派生以下类型类:

- `Num`
- `Semigroup`
- `Monoid`

这是什么意思？嗯，`Num`实际上是为`Term s a`实现的，其中`a`是Plutarch类型。例如，如果你想为`Term s PPubKeyHash`实现`Semigroup`(`Term s PByteString`已经有一个`Semigroup` 实例)，你可以这样写:

```hs
{-# LANGUAGE StandaloneDeriving #-}

deriving via (Term s (DerivePNewtype PPubKeyHash PByteString)) instance Semigroup (Term s PPubKeyHash)
```
