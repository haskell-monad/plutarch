# Deriving typeclasses with generics

Plutarch还为完全自定义的类型提供了复杂的泛型派生支持。特别是，您可以轻松地为您自己的类型派生`PlutusType`:

```hs
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude

data MyType (a :: PType) (b :: PType) (s :: S)
  = One (Term s a)
  | Two (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)
```

> 注意: 这需要`generics-sop`包.

This will use a [Scott encoding representation](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding) for `MyType`, which is typically what you want. If you want to use [data encoding representation](./../Concepts/Data%20and%20Scott%20encoding.md) instead in your `PlutusType` instance - you should derive it using `PIsDataReprInstances`. Check out: [implementing `PIsDataRepr` and friends](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)

这将为`MyType`使用[Scott编码表示](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding)，这通常是您想要的。如果您想在`PlutusType`实例中使用[Data编码表示](./../Concepts/Data%20and%20Scott%20encoding.md) - 您应该使用`PIsDataReprInstances` 派生它。查看：[实现`PIsDataRepr`和friends](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)

目前，泛型派生支持以下类型类:

- [`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding) (仅Scott编码)
- [`PIsDataRepr`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)
