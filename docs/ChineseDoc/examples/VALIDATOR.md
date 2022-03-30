用Plutarch编写的验证器和铸币策略示例。

> 注意：如果您发现任何错误/有任何本指南无法回答的相关问题，请不要犹豫，立即提出问题。我们的目标是为Plutarch用户提供高质量的文档！

- [始终成功的验证器](#始终成功的验证器)
- [始终失败的验证器](#始终失败的验证器)
- [检查签名者中是否存在值的验证器](#检查签名者中是否存在值的验证器)
- [在验证器中使用自定义datum/redeemer](#在验证器中使用自定义datumredeemer)

> 另外: 一定要先看看[编译和运行](./../GUIDE.md#compiling-and-running)！

# 始终成功的验证器

```hs
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Scripts

alwaysSucceeds :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \datm redm ctx -> pconstant ()
```

所有参数都被忽略。所以我们使用通用的`PDatum`和`PRedeemer`类型。

执行:

```hs
import qualified PlutusTx

> alwaysSucceeds `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf unit ()))))
```

# 始终失败的验证器

```hs
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Scripts

alwaysFails :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
alwaysFails = plam $ \datm redm ctx -> perror
```

类似于上面的例子.

执行:

```hs
import qualified PlutusTx

> alwaysFails `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
Left (EvaluationError [] "(CekEvaluationFailure,Nothing)")
```

# 检查签名者中是否存在值的验证器

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Crypto
import Plutarch.Api.V1.Scripts
import qualified Plutarch.Monadic as P

checkSignatory :: Term s (PPubKeyHash :--> PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
checkSignatory = plam $ \ph _ _ ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatch ctx.purpose
  let signatories = pfield @"signatories" # ctx.txInfo
  pif
    (pelem # pdata ph # pfromData signatories)
    -- 成功!
    (pconstant ())
    -- 签名不存在.
    perror
```

> 注意: 上面的代码片段使用了GHC9特性(`QualifiedDo`和`OverloadedRecordDot`).请务必查看[Do syntax with `TermCont`](./../Usage/Do%20syntax%20with%20TermCont.md)和[alternatives to `OverloadedRecordDot`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#alternatives-to-overloadedrecorddot).

我们匹配脚本的目的，看看它是否真的用于`spending`(支出)- 我们从`txInfo`(第7个字段)中获取signatories字段，检查给定的公钥hash是否存在于`signatories`中，就是这样！

在将`checkSignatory`视为验证器脚本之前，我们传递`PPubKeyHash`很重要。

```hs
{-# LANGUAGE OverloadedStrings #-}

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval
import qualified PlutusTx

hashStr :: PubKeyHash
hashStr = "abce0f123e"

pubKeyHash :: Term s PPubKeyHash
pubKeyHash = pconstant hashStr

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
      [fromString hashStr, "f013", "ab45"]
      mempty
      ""
    )
    (Spending (TxOutRef "" 1))

> evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf unit ()))))
```

# 在验证器中使用自定义datum/redeemer

你所要做的就是为你的自定义datum/redeemer[实现`PIsDataRepr`和friends](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)，你就可以像你的验证器中的`PScriptContext`一样使用它！
