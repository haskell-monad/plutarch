# The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`

当为Plutarch类型[实现`PIsDataRepr`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)时，如果Plutarch类型也有一个使用[`makeIsDataIndexed`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#v:makeIsDataIndexed)的[Haskell synonym](./../Concepts/Haskell%20Synonym.md)(例如，`ScriptContext`是`PScriptContext`的Haskell同义词) - 你必须确保构造函数顺序是正确的。

特别是，使用`makeIsDataIndexed`，你可以将`indices`(索引)分配给你的Haskell ADT的构造函数。这决定了ADT在Plutus Core中的表示方式。确保相应的Plutarch类型知道这些索引非常重要，这样它才能正确解码ADT - 如果您通过Haskell将其传递到Plutarch代码中。

例如，考虑`Maybe`。Plutus将这些索引分配给它的构造函数:

```hs
makeIsDataIndexed ''Maybe [('Just, 0), ('Nothing, 1)]
```

0到`Just`，1到`Nothing`。所以对应的Plutarch类型，`PMaybeData`被定义为:

```hs
data PMaybeData a (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
```

如果将其定义为如下方式则会有非常不易察觉的错误:

```hs
data PMaybeData a (s :: S)
  = PDNothing (Term s (PDataRecord '[]))
  | PDJust (Term s (PDataRecord '["_0" ':= a]))
```

构造器顺序错误！

重要的不仅仅是构造函数排序 - 字段排序也很重要! 虽然这是不言自明的。注意`PTxInfo`如何与其Haskell同义词`TxInfo`共享完全相同的字段顺序。

```hs
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "outputs" ':= PBuiltinList (PAsData PTxOut)
               , "fee" ':= PValue
               , "mint" ':= PValue
               , "dcert" ':= PBuiltinList (PAsData PDCert)
               , "wdrl" ':= PBuiltinList (PAsData (PTuple PStakingCredential PInteger))
               , "validRange" ':= PPOSIXTimeRange
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash)
               , "data" ':= PBuiltinList (PAsData (PTuple PDatumHash PDatum))
               , "id" ':= PTxId
               ]
          )
      )
```

```hs
data TxInfo = TxInfo
  { txInfoInputs      :: [TxInInfo]
  , txInfoOutputs     :: [TxOut]
  , txInfoFee         :: Value
  , txInfoMint        :: Value
  , txInfoDCert       :: [DCert]
  , txInfoWdrl        :: [(StakingCredential, Integer)]
  , txInfoValidRange  :: POSIXTimeRange
  , txInfoSignatories :: [PubKeyHash]
  , txInfoData        :: [(DatumHash, Datum)]
  , txInfoId          :: TxId
  }
```

字段名称并不重要。它们只是运行时不存在的标签。

## What about `newtype`s?

当然，当您使用`newtype`派生(例如 `derive newtype ...`)为PlutusTx类型派生`FromData`或`ToData`时，这并不适用。在这种情况下，`Data`表示与内部类型完全相同。

```hs
import qualified PlutusTx
import PlutusTx.Prelude

newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: BuiltinByteString }
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
```

例如，在这里，`CurrencySymbol`具有与`BuiltinByteString`完全相同的`Data`表示。没有添加额外的信息。