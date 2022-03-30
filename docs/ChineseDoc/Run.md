本文档描述了如何编译和运行Plutarch —— 无论是用于链上部署还是链下测试。

> 注意：如果您发现有任何错误/任何本指南无法回答的相关问题，请不要犹豫，立即提出问题。我们的目标是为Plutarch用户提供高质量的文档！

<details>
<summary> 目录 </summary>

- [通用扩展和GHC选项](#通用扩展和ghc选项)
- [评估](#评估)

</details>

# 通用扩展和GHC选项

您通常希望遵守[Plutarch repo](https://github.com/Plutonomicon/plutarch/blob/master/plutarch.cabal)使用的相同扩展和GHC选项。

<details>
<summary> GHC扩展列表 </summary>

- `NoStarIsType`
- `BangPatterns`
- `BinaryLiterals`
- `ConstrainedClassMethods`
- `ConstraintKinds`
- `DataKinds`
- `DeriveAnyClass`
- `DeriveDataTypeable`
- `DeriveFoldable`
- `DeriveFunctor`
- `DeriveGeneric`
- `DeriveLift`
- `DeriveTraversable`
- `DerivingStrategies`
- `DerivingVia`
- `DoAndIfThenElse`
- `EmptyCase`
- `EmptyDataDecls`
- `EmptyDataDeriving`
- `ExistentialQuantification`
- `ExplicitForAll`
- `FlexibleContexts`
- `FlexibleInstances`
- `ForeignFunctionInterface`
- `GADTSyntax`
- `GeneralisedNewtypeDeriving`
- `HexFloatLiterals`
- `ImplicitPrelude`
- `InstanceSigs`
- `KindSignatures`
- `LambdaCase`
- `MonomorphismRestriction`
- `MultiParamTypeClasses`
- `NamedFieldPuns`
- `NamedWildCards`
- `NumericUnderscores`
- `OverloadedStrings`
- `PartialTypeSignatures`
- `PatternGuards`
- `PolyKinds`
- `PostfixOperators`
- `RankNTypes`
- `RelaxedPolyRec`
- `ScopedTypeVariables`
- `StandaloneDeriving`
- `StandaloneKindSignatures`
- `TraditionalRecordSyntax`
- `TupleSections`
- `TypeApplications`
- `TypeFamilies`
- `TypeOperators`
- `TypeSynonymInstances`
- `ViewPatterns`

</details>

# 评估

您可以使用`compile`(来自 `Plutarch`模块)编译Plutarch term，确保它没有自由变量。[`Plutus.V1.Ledger.Scripts`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html)中的API应该会很有帮助。

> 要进一步了解编译的内容 - 您可以使用`printTerm`或`printScript`(来自`Plutarch`模块)。

我经常使用这些辅助函数来快速测试Plutarch：

```haskell
import Data.Text (Text)
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import Plutus.V1.Ledger.Api (ExBudget)
import Plutus.V1.Ledger.Scripts (Script (unScript), ScriptError, applyArguments)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)

eval :: ClosedTerm a -> Either ScriptError (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
eval x = fmap (\(a, b, s) -> (a, b, unScript s)) . evaluateScript $ compile x

evalWithArgs :: ClosedTerm a -> [Data] -> Either ScriptError (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
evalWithArgs x args = fmap (\(a, b, s) -> (a, b, unScript s)) . evaluateScript . flip applyArguments args $ compile x
```

结果三元组中的字段分别对应于执行预算(使用了多少内存和CPU单元)、跟踪日志和脚本结果。通常你只对脚本结果感兴趣，那么你可以使用：

```haskell
evalT :: ClosedTerm a -> Either ScriptError (Program DeBruijn DefaultUni DefaultFun ())
evalT x = fmap (\(_, _, s) -> unScript s) . evaluateScript $ compile x

evalWithArgsT :: ClosedTerm a -> [Data] -> Either ScriptError (Program DeBruijn DefaultUni DefaultFun ())
evalWithArgsT x args = fmap (\(_, _, s) -> unScript s) . evaluateScript . flip applyArguments args $ compile x
```

> 注意: 您几乎可以忽略此处涉及的UPLC类型。它真正的意思是结果是一个"UPLC程序"。当它被打印出来时，它非常清晰 —— 尤其是用于调试目的。对于使用`Plutarch`它虽然是没有必要的，但您可能会发现[Plutonomicon UPLC指南](https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md)很有用。