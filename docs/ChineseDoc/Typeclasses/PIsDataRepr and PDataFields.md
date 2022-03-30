# `PIsDataRepr` & `PDataFields`

`PIsDataRepr`允许轻松构造和解构`Constr` [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)值。它允许对[`Data`编码](./../Concepts/Data%20and%20Scott%20encoding.md)值进行完全类型安全匹配，而无需在生成的脚本中嵌入类型信息 - 与 PlutusTx不同。最重要的是，`PDataFields`允许进行符合人体工程学的字段访问。

> 旁白: 什么是`Constr`数据值？简而言之，这就是Plutus Core如何将有意义的ADT编码为`Data`/`BuiltinData`。它本质上是一种`sum-of-products`编码。但是你不必太在意这些。本质上，只要您有一个自定义的有意义的ADT(不仅仅是一个整数、bytestring、字符串/文本、列表或关联映射) - 并且您想将其表示为Data编码值 - 您应该为它实现`PIsDataRepr`。

例如，`PScriptContext` - 它是[`ScriptContext`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptContext)的Plutarch同义词 - 具有必要的实例。这让您可以轻松地跟踪它的类型、匹配它、解构它 —— 您命名它！

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import qualified Plutarch.Monadic as P

foo :: Term s (PScriptContext :--> PString)
foo = plam $ \ctx -> P.do
  purpose <- pmatch pfield @"purpose" # ctx
  case purpose of
    PMinting _ -> "It's minting!"
    PSpending _ -> "It's spending!"
    PRewarding _ -> "It's rewarding!"
    PCertifying _ -> "It's certifying!"
```

> 注意：上面的代码片段使用了GHC9特性(`QualifiedDo`)。请务必查看[Do syntax with `TermCont`](./../Usage/Do%20syntax%20with%20TermCont.md)。

当然，就像`ScriptContext` - `PScriptContext`在Plutus Core中表示为`Data`值。Plutarch只是让您跟踪它在类型系统中的确切表示。

以下是`PScriptContext`的定义方式:

```hs
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "purpose" ':= PScriptPurpose
               ]
          )
      )
```

它是一个包含[`PDataRecord`](./../Types/PDataSum%20and%20PDataRecord.md)term的构造函数。它有2个字段 - `txInfo`和`purpose`。

首先，我们使用`pfield @"purpose"`提取`purpose`字段:

```hs
pfield :: Term s (PScriptContext :--> PScriptPurpose)
```

> 注意: 当从同一个变量中提取多个字段时，您应该使用`pletFields`。请参阅: [提取字段](#all-about-extracting-fields)

> 另外: `pfield`实际上是返回类型多态。它可以返回`PAsData PScriptPurpose`和`PScriptPurpose`。在这种情况下，GHC正确地推断出我们实际上想要一个`PScriptPurpose`，因为`pmatch`在`PAsData PScriptPurpose`上不起作用！
>
> 有时GHC并不那么聪明，您不得不提供显式类型注释。或者你可以简单地使用`pfromData $ pfield ....`。

现在，我们可以在`Term s PScriptPurpose`上`pmatch`以从Plutarch term中提取Haskell ADT(`PScriptPurpose s`):

```hs
pmatch :: Term s PScriptPurpose -> (PScriptPurpose s -> Term s PString) -> Term s PString
```

现在我们有了`PScriptPurpose s`，我们可以只用`case`匹配它! `PScriptPurpose`定义为:

```hs
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))
```

这只是一个Plutarch sum类型。

我们对字段(`PDataRecord` term)并不真正感兴趣，因此我们只需将构造函数与熟悉的`case`进行匹配。简单!

让我们将`ScriptContext`作为`Data`值从Haskell传递给这个Plutarch脚本，看看它是否有效!

```hs
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval
import qualified PlutusTx

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
      mempty
      mempty
      ""
    )
    (Minting (CurrencySymbol ""))

> foo `evalWithArgsT` [PlutusTx.toData mockCtx]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf string "It's minting!"))))
```

> 另外: 您可以在[编译和运行](../GUIDE.md#compiling-and-running)找到`evalWithArgsT`的定义。

## All about extracting fields

由于`pfield`，我们在上面的示例中瞥见了字段提取。然而，这几乎没有触及表面。

一旦一个类型有一个`PDataFields`实例，就可以使用以下3个函数来完成字段提取:

- `pletFields`
- `pfield`
- `hrecField` (不使用`OverloadedRecordDot`或者[record dot preprocessor](https://hackage.haskell.org/package/record-dot-preprocessor)时)

每个都有自己的目的。然而，`pletFields`可以说是最通用和最有效的。每当您需要从同一个变量中提取多个字段时，您应该使用`pletFields`:

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import qualified Plutarch.Monadic as P

foo :: Term s (PScriptContext :--> PUnit)
foo = plam $ \ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  let
    purpose = ctx.purpose
    txInfo = ctx.txInfo
  -- <use purpose and txInfo here>
  pconstant ()
```

> 注意: 上面的代码片段使用了GHC9特性(`QualifiedDo`和`OverloadedRecordDot`)。请务必查看[Do syntax with `TermCont`](./../Usage/Do%20syntax%20with%20TermCont.md)和[alternatives to `OverloadedRecordDot`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#alternatives-to-overloadedrecorddot)。

本质上，`pletFields`接受你想要访问的字段名称的类型级别列表和接受`HRec`的延续函数。这个`HRec`本质上是绑定字段的集合。你不必太担心`HRec`的细节。这种特殊用法具有以下类型:

```hs
pletFields :: Term s PScriptContext
  -> (HRec
        (BoundTerms
           '[ "txInfo" ':= PTxInfo, "purpose" ':= PScriptPurpose]
           '[ 'Bind, 'Bind]
           s)
      -> Term s PUnit)
  -> Term s PUnit
```

然后，您可以使用`OverloadedRecordDot`访问此`HRec`上的字段。

接下来是`pfield`。如果您只想要一个变量中的一个字段而不是更多，您应该只使用它。它的用法只是`pfield @"fieldName" # variable`。但是，在这种情况下，您也可以使用`pletFields`(例如，`pletFields @'["fieldName"] variable`)。带有单数字段的 `pletFields`与`pfield`具有相同的效率！

最后，`hrecField`只是为了补充记录点语法的不足。请参阅: [替代`OverloadedRecordDot`](#alternatives-to-overloadedrecorddot)。

> 注意: 要意识到的重要一点是`pfield`和`hrecField`(或`HRec`上的重载Record Dot)是返回类型多态的。它们可以返回`PAsData Foo`或`Foo` terms，具体取决于周围的上下文。这在`pmatch`的情况下非常有用，因为`pmatch`不适用于`PAsData` term。所以你可以简单地写`pmatch $ pfield ...`并且`pfield`会正确地选择解开`PAsData` term。

### Alternatives to `OverloadedRecordDot`

如果`OverloadedRecordDot`不可用，您也可以尝试使用[record dot预处理器插件](https://hackage.haskell.org/package/record-dot-preprocessor).

如果你不想使用任何一个，你可以简单地使用`hrecField`。事实上，上面的`ctx. purpose`只是翻译成`hrecField @" purpose" ctx`。那里没有什么神奇的！

## All about constructing data values

我们了解了类型安全匹配(通过`PlutusType`)以及类型安全字段访问(通过`PDataFields`) —— 构造怎么样？由于`PIsDataRepr`允许您派生[`PlutusType`](./PlutusType,%20PCon,%20and%20PMatch.md)，并且`PlutusType`不仅赋予了解构能力也赋予了你构造值的能力 - 你可以一样容易的做到这一点！

让我们看看如何在给定`PCurrencySymbol`的情况下构建`PMinting` `PScriptPurpose`:

```hs
import Plutarch.Prelude
import Plutarch.Api.V1

currSym :: Term s PCurrencySymbol
```

```hs
purpose :: Term s PScriptPurpose
purpose = pcon $ PMinting fields
  where
    currSymDat :: Term _ (PAsData PCurrencySymbol)
    currSymDat = pdata currSym
    fields :: Term _ (PDataRecord '[ "_0" ':= PCurrencySymbol ])
    fields = pdcons # currSymDat # pdnil
```

所有类型注释都在这里提供帮助!

这就像您[来自`PlutusType`/`PCon`](./PlutusType,%20PCon,%20and%20PMatch.md)的常规`pcon`用法。它接受您的Plutarch类型的Haskell ADT并返回一个Plutarch term。

更有趣的是`fields`绑定。回想一下，`PMinting`是一个带有一个参数的构造函数，该参数是一个[`PDataRecord`](../Types/PDataSum%20and%20PDataRecord.md)term。特别是，我们想要: `Term s (PDataRecord '["_0" ':= PCurrencySymbol ])`。它对字段的确切类型、位置和名称进行编码。所以，我们所要做的就是创建一个 `PDataRecord` term！

当然，我们使用`pdcons`来做到这一点 —— 这只是熟悉的`cons`，专门用于`PDataRecord` terms。

```hs
pdcons :: forall label a l s. Term s (PAsData a :--> PDataRecord l :--> PDataRecord ((label ':= a) ': l))
```

It takes a `PAsData a` and adds that `a` to the `PDataRecord` heterogenous list. We feed it a `PAsData PCurrencySymbol` term and `pdnil` - the empty data record. That should give us:
它需要一个`PAsData a`并将该`a`添加到`PDataRecord`异构列表中。我们为它提供一个`PAsData PCurrencySymbol` term和`pdnil` - 空数据记录。这应该给我们:

```hs
pdcons # currSymDat # pdnil :: Term _ (PDataRecord '[ label ':= PCurrencySymbol ])
```

凉爽的! 等等，什么是`label`？它是与字段关联的字段名称，在我们的例子中，我们希望字段名称`_0` - 因为这是`PMinting`构造函数想要的。您可以使用类型应用程序指定标签，也可以只为绑定提供类型注释(这就是我们在这里所做的)。或者您可以让GHC尝试将 `label`与周围环境进行匹配!

现在我们有了`fields`，我们可以将它与`PMinting`一起使用来构建`PScriptPurpose s`并将其提供给`pcon` - 我们完成了！

## Implementing `PIsDataRepr` and friends

使用泛型派生和`PIsDataReprInstances`来实现这些是相当简单的。您所需要的只是使用`PDataRecord`的格式良好的类型。例如，假设你想为这个Haskell类型的Plutarch版本实现`PIsDataRepr`:

```hs
data Vehicle
  = FourWheeler Integer Integer Integer Integer
  | TwoWheeler Integer Integer
  | ImmovableBox
```

您将相应的Plutarch类型声明为:

```hs
import Plutarch.Prelude

data PVehicle (s :: S)
  = PFourWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger, "_2" ':= PInteger, "_3" ':= PInteger]))
  | PTwoWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger]))
  | PImmovableBox (Term s (PDataRecord '[]))
```

每个字段类型还必须有一个`PIsData`实例。我们已经满足了上述这些标准，因为`PInteger`确实有一个`PIsData` 实例。但是，以`PBuiltinList`为例。`PBuiltinList`的`PIsData`实例仅限于`PAsData`元素。

```hs
instance PIsData a => PIsData (PBuiltinList (PAsData a))
```

因此，您可以使用`PBuiltinList (PAsData PInteger)`作为字段类型，但不能使用`PBuiltinList PInteger`。

> 注意: `PVehicle`中的构造函数排序很重要！如果您在`Vehicle`上使用[`makeIsDataIndexed`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#v:makeIsDataIndexed)为每个构造函数分配索引 - Plutarch类型的构造函数必须遵循相同的索引顺序。
>
> 在该例子中，`PFourWheeler`位于第0个索引，`PTwoWheeler`位于第1个索引，`PImmovableBox`位于第3个索引。因此，相应的`makeIsDataIndexed`用法应该是:
> 
> ```hs
> PlutusTx.makeIsDataIndexed ''FourWheeler [('FourWheeler,0),('TwoWheeler,1),('ImmovableBox,2)]
> ```
>
> 另请参阅: [Haskell ADT和`PIsDataRepr`之间的同构](./../Tricks/makeIsDataIndexed,%20Haskell%20ADTs,%20and%20PIsDataRepr.md)

您只需使用泛型派生`PIsDataRepr`。但是，您还必须使用`PIsDataReprInstances`派生`PIsData`和`PlutusType`。对于单一构造函数数据类型，您还应该派生`PDataFields`。

结合所有这些，您将拥有:

```hs
{-# LANGUAGE UndecidableInstances #-}

import qualified GHC.Generics as GHC
import Generics.SOP

import Plutarch.Prelude
import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances))

data PVehicle (s :: S)
  = PFourWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger, "_2" ':= PInteger, "_3" ':= PInteger]))
  | PTwoWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger]))
  | PImmovableBox (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PVehicle
```

> 注意: 您不能为使用[Scott encoding](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding)表示的类型派生`PIsDataRepr`。您的类型必须格式正确，并且应该使用 `PDataRecord` terms。

是的! 现在您可以将`PVehicle`表示为`Data`值，并以超级符合人体工程学的方式解构和访问其字段。让我们试试吧!

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Plutarch.Prelude
import qualified Plutarch.Monadic as P

test :: Term s (PVehicle :--> PInteger)
test = plam $ \veh' -> P.do
  veh <- pmatch veh'
  case veh of
    PFourWheeler fwh' -> P.do
      fwh <- pletFields @'["_0", "_1", "_2", "_3"] fwh'
      fwh._0 + fwh._1 + fwh._2 + fwh._3
    PTwoWheeler twh' -> P.do
      twh <- pletFields @'["_0", "_1"] twh'
      twh._0 + twh._1
    PImmovableBox _ -> 0
```

> 注意: 上面的代码片段使用了GHC9特性(`QualifiedDo`和`OverloadedRecordDot`). 请务必查看[Do syntax with `TermCont`](./../Usage/Do%20syntax%20with%20TermCont.md)和[alternatives to `OverloadedRecordDot`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#alternatives-to-overloadedrecorddot).

那么使用单数构造函数的类型呢？这与sum类型的情况非常相似。下面是它的外观:

```hs
{-# LANGUAGE UndecidableInstances #-}

import qualified GHC.Generics as GHC
import Generics.SOP

import Plutarch.Prelude
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )

newtype PFoo (s :: S) = PMkFoo (Term s (PDataRecord '["foo" ':= PByteString]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PFoo
```

与sum类型的使用相比，只是一个额外的`PDataFields`推导！
