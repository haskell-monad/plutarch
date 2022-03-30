# Prefer statically building constants whenever possible
尽可能选择静态构建常量

每当您可以从纯Haskell值中构建Plutarch常量时 - 就去做吧！诸如`pconstant`、`phexByteStr`之类的函数在Plutarch类型的常规[Haskell synonyms](./../Concepts/Haskell%20Synonym.md)上运行。与可能适用于Plutarch term的`pcon`不同(例如，`pcon $ PJust x`，`x`是`Term s a`)。Plutarch term是一个完全"运行时"的概念。"运行时"与"Plutus Core运行时"中的一样。他们只在运行时被评估！

另一方面，每当您使用`pconstant`、`phexByteStr`等将Haskell同义词转换为其对应的Plutarch类型时，您都在直接构建Plutus Core常量。这完全是静态的！没有运行时函数调用，没有运行时构建，它就在那里，在已编译的脚本中。

这是一个示例，假设您要构建一个 `PScriptPurpose` - `PMinting "f1e301"`。你认为哪个片段更好？

```hs
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Value

import Plutus.V1.Ledger.Api

pconstant (Minting "f1e301")
-- (or)
let currSym = pcon $ PCurrencySymbol $ phexByteStr "f1e301"
 in pcon $ PMinting $ pdcons # pdata currSym # pdnil
```

语义是相同的。但前者(`pconstant`)直接编译为常数项。而后者编译为一些在Plutus Core运行时构建常量的代码。

> 旁白: 请记住，Haskell运行时实际上是Plutarch的编译时！即使在Haskell世界中有一个动态计算的变量，它在Plutarch世界中仍然是一个常数。因此，您可以将它用作`pconstant`的参数来使用！

每当您需要从Haskell值构建`a`类型的Plutarch term时，请使用`pconstant`。每当您需要构建类型为`PAsData a`的Plutarch term时，请使用`pconstantData`！