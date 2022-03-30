# `PlutusType`, `PCon`, and `PMatch`

`PlutusType`是确定Plutarch类型的底层表示的主要类型类。它允许您从Plutarch类型的构造函数(可能包含其他Plutarch terms)构造和解构Plutus Core常量。它本质上是`PCon`(用于term构造)和`PMatch`(用于term解构)的组合。

```hs
class (PCon a, PMatch a) => PlutusType (a :: k -> Type) where
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s b. a s -> Term s (PInner a b)
  pmatch' :: forall s b. Term s (PInner a b) -> (a s -> Term s b) -> Term s b
```

> 注意: 您不需要过多地研究类型！毕竟，你将使用 `pcon` 和 `pmatch`，而不是 `pcon'` 和 `pmatch'`。
> `PInner`表示`a`的"内部"类型 - 表示Plutus Core常量的Plutarch类型，用于表示`a`。

这是`PMaybe`的`PlutusType`实例:

```hs
data PMaybe a s = PJust (Term s a) | PNothing

instance PlutusType (PMaybe a) where
  type PInner (PMaybe a) b = (a :--> b) :--> PDelayed b :--> b
  pcon' (PJust x) = plam $ \f (_ :: Term _ _) -> f # x
  pcon' PNothing = plam $ \_ g -> pforce g
  pmatch' x f = x # (plam $ \inner -> f (PJust inner)) # (pdelay $ f PNothing)
```

这是[熟悉的`Maybe`数据类型的Scott编码表示](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding).可以看到，`PMaybe`的`PInner`实际上是一个Plutarch级别的函数。这正是`pcon'`创建函数的原因。然后，`pmatch'`只是简单地"匹配"函数 - Scott编码方式。

你应该总是使用`pcon`和`pmatch`而不是`pcon'`和`pmatch'` —— 这些是由`PCon`和`PMatch`类型类提供的:

```hs
class PCon a where
  pcon :: a s -> Term s a

class PMatch a where
  pmatch :: Term s a -> (a s -> Term s b) -> Term s b
```

所有`PlutusType`实例都免费获得`PCon`和`PMatch`实例!

对于不能同时是`PCon`和`PMatch`的类型 - 随意实现其中一个！但是，一般来说，更喜欢实现`PlutusType`！

`PlutusType`实例的另一个特点是您可以提取任何`PlutusType`实例的内部类型！上面，`PMaybe`的内部类型(或表示)是一个函数。您可以使用`pto`安全地将这种内部类型取出 -

```hs
pto :: Term s a -> (forall b. Term s (PInner a b))
```

这在使用`newtype`时非常有用。请注意，例如，`PCurrencySymbol`只是`PByteString`的`newtype`。它的`PInner`也是`PByteString`。为了能够在`PCurrencySymbol`中使用对`PByteString`s进行操作的函数，您可以简单地使用`pto`取出`PByteString`!

## Implementing `PlutusType` for your own types (Scott Encoding)

如果您想用[Scott encoding](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding)表示您的数据类型(因此不需要将其设为`Data`编码),你应该简单地推导出它:

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

> 注意: 这个需要`generics-sop`包.

## Implementing `PlutusType` for your own types (`Data` Encoding)

如果您的类型应该使用[`Data` encoding](./../Concepts/Data%20and%20Scott%20encoding.md#data-encoding)来表示(即有一个[`PIsDataRepr`](./PIsDataRepr%20and%20PDataFields.md)实例)，您可以通过`PIsDataReprInstances`导出 `PlutusType`:

```hs
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude
import Plutarch.DataRepr (PIsDataReprInstances(PIsDataReprInstances))

data MyType (a :: PType) (b :: PType) (s :: S)
  = One (Term s (PDataRecord '[ "_0" ':= a ]))
  | Two (Term s (PDataRecord '[ "_0" ':= b ]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances (MyType a b)
```

看: [实现`PIsDataRepr` and friends](./PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends).

## Implementing `PlutusType` for your own types (`newtype`)

看: [`DerivePNewtype`](./../Usage/Deriving%20for%20newtypes.md).
