想要为Plutarch做出贡献? 从安全界面寻找Plutarch当前未提供的功能? 你来对地方了!

> 注意: 如果您发现任何错误/有任何本指南无法回答的相关问题，请不要犹豫，立即提出问题。我们的目标是为 Plutarch 开发人员提供高质量的文档!

<details>
<summary> 目录 </summary>

- [Code Style](#code-style)
- [预提交检查](#预提交检查)
- [更新变更日志](#更新变更日志)
- [PR的目标分支](#pr的目标分支)
- [概念](#概念)
  - [Plutus Core常量(UNSAFE)](#plutus-core常量unsafe)
  - [Plutus core内建函数](#plutus-core内建函数)
  - [使用BuiltinData/Data/PData](#使用builtindatadatapdata)
  - [`PConstant`和`PLift`](#pconstant和plift)
- [低级别示例](#低级别示例)
  - [手动从`ScriptContext`中提取`txInfoInputs`(UNTYPED)](#手动从scriptcontext中提取txinfoinputsuntyped)
- [有用链接](#有用链接)

</details>

# Code Style

您通常应该遵循[MLabs样式指南](https://github.com/mlabs-haskell/styleguide)，感谢[@Koz Ross](https://github.com/kozross)。

**不鼓励的扩展**

- `ImportQualifiedPost`
- `RecordWildCards`

# 预提交检查

记得运行`./bin/format`来格式化你的代码和`cabal test`，以及`cabal test -f development`，以确保在进行PR之前所有测试都通过！

# 更新变更日志

如果您的PR对某些面向用户的功能进行了更改 - 请总结更改并将其添加到 `CHANGELOG.md`。

# PR的目标分支

通常情况下，您会直接向`master`提交PR。
但是，有时，存在一个发布周期，并且存储库的状态在不断变化。在此期间，通常会有一个`master <- staging`PR打开。只要 `staging`PR是打开的，你就应该将大多数新分支建立在它之上并合并回它。错误修复, 对于`master`中存在的错误，不受此要求的约束。

# 概念

即使面向公众的API缺少某些功能 —— 您始终可以使用诸如`punsafeConstant`和`punsafeBuiltin`之类的函数来实现它们——这些函数允许您在Plutus核心和Plutarch之间穿行。

对`Plutus core`的全面熟悉很重要。您可以通过以下文档了解所有这些内容:

- [Builtin lists](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md)
- [Builtin pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md)
- [Builtin functions](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-functions.md)
- [Builtin data](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)

[Pluto指南](https://github.com/Plutonomicon/pluto/blob/main/GUIDE.md)的部分内容也可能有用。

## Plutus Core常量(UNSAFE)

> **注意**: 在存在 `pconstant` 的情况下，几乎不需要以下信息。请参阅Plutarch用户指南的[常量构建](./GUIDE.md#constants) 和[`PConstant`/`PLift`](./GUIDE.md#pconstant--plift) 部分。

通常，您需要构建Plutus核心常量。您可以使用`Some`和`ValueOf`来做到这一点。以下是`pcon PTrue`如何创建Plutarch term，该term实际计算为代表布尔值的Plutus核心常量:

```haskell
import qualified PlutusCore as PLC

pcon' PTrue = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool True
pcon' PFalse = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool False
```

这里有很多东西要解开 - 但一般模式总是一样的。第一步是构造Plutus核心常数:

```haskell
PLC.Some $ PLC.ValueOf PLC.DefaultUniBool True
```

创建其他常量时唯一需要更改的部分是类型和值。这里的类型是`DefaultUniBool`。这意味着下一个参数必须是`Bool`。由类型系统确保 - 你不用担心 :)

您可以浏览默认Universe中的其他类型(您将使用的类型)。你能猜出如何从Haskell字符串中创建Plutus核心字符串，并将其表示为Plutarch term吗?

```haskell
import qualified Data.Text as Txt
import qualified PlutusCore as PLC

punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniString . Txt.pack
```

(它甚至是pointfree!)

这基本上就是`Term s PString`的`IsString`实现所做的。这就是字符串字面量最终成为plutus核心内置字符串的方式。

再来一个复杂的例子 —— `DefaultUniProtoList`. 这是一个内置列表, 但元素类型是什么呢? 好吧，你必须自己指定！使用`DefaultUniApply`在`DefaultUniProtoList`上"应用"类型(来自默认范围`universe`)

```haskell
import qualified PlutusCore as PLC

PLC.Some . PLC.ValueOf (PLC.DefaultUniProtoList `PLC.DefaultUniApply` PLC.DefaultUniInteger)
```

它将一个`[Integer]`转换为一个Plutus核心内置整数列表. 方便的!

实际上，``DefaultUniProtoList `DefaultUniApply` a`` 有一个方便的`pattern`同义词 - `DefaultUniList a`。 使用它，您可以将上述内容简化为:

```haskell
PLC.Some . PLC.ValueOf (PLC.DefaultUniList PLC.DefaultUniInteger)
```

请注意，您必须自己提供正确的类型注释，因为`punsafeConstant`只是推断为 `Term s a`。这就是它不安全的原因！使用此不安全函数时，请确保提供正确的注释:

```haskell
foo :: Bool -> Term s PBool
foo = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniBool
```

当然，我们在Plutarch中将Plutus核心布尔值表示为`Term s PBool` —— 这就是它的类型!

## Plutus core内建函数

这是你最争论的问题。内置函数将成为您所做的一切的基础。但是它们的文档是...稀疏的。

您可以使用 `punsafeBuiltin`为Plutus核心内置函数创建Plutarch同义词。它从Plutus核心内置函数创建Plutarch级别的函数。

让我们尝试做一个，`AddInteger` 怎么样?

```haskell
import qualified PlutusCore as PLC

addI :: Term s (PInteger :--> PInteger :--> PInteger)
addI = punsafeBuiltin PLC.AddInteger
```

就像`punsafeConstant`，你必须自己提供正确的注解。我们知道`AddInteger`接受两个Plutus核心内置整数并返回另一个。我们在Plutarch中使用`PInteger terms`来表示这些整数 - 所以我们开始了!

您可以像使用其他任何函数一样使用和应用此Plutarch函数。

现在这里是它偏离轨道的地方，一些内置函数需要使用`forces`。这些内置函数具有内在的多态类型变量。您需要`force`它们的次数取决于它们拥有的类型变量的数量。

让我们看一个例子 - `HeadList`。它的类型可以被认为是 - `forall a. [a] -> a`。它有一个类型变量，因此需要被强制执行一次:

```haskell
pheadBuiltin :: Term s (PBuiltinList a :--> a)
pheadBuiltin = pforce $ punsafeBuiltin PLC.HeadList
```

我们使用`pforce`强制一个Plutarch term，回想一下`punsafeBuiltin`返回一个term。当然，您需要自己输入所有内容(You need to type it all yourself of course)。`pforce`并不意味着您需要摆脱Plutarch级别类型中的类型变量。它仍然适用于任何`a` - 强制只需要发生在调用的位置。

你可以盲目地这样做，`HeadList`需要1个`force`，所以只需要`pforce`一次. `TailList`也需要1个`force`. `ChooseList`需要2个`force`(`forall a b. [a] -> b -> b -> b`)。以下是您将如何为它实现 Plutarch 同义词的方法:

```haskell
pchooseList :: Term s (PBuiltinList a :--> b -> b -> b)
pchooseList = pforce $ pforce $ punsafeBuiltin PLC.ChooseList
```

> 旁白：您还应该在这里`hoist`同义词(它接收一个或多个`forces`)！

我们有一个[Plutus Core内置函数参考](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-functions.md)供您了解有关它们的所有信息，包括类型、用法和`forcing`。

## 使用BuiltinData/Data/PData

大多数时候，您将使用`BuiltinData`/`Data` - 这是将从外部传递到你的脚本的参数类型。这是`datum`、`redeemer`和`script context`的类型。 这也是您可以传递给`Script`的参数类型。

Plutarch旨在向用户隐藏这些低级细节。理想情况下，您将使用 `PDataSum`/`PDataList` 和 `PAsData` - 它们本质上只是 `BuiltinData`，但它是在Plutarch级别被类型的。

但是，如果您想直接使用`BuiltinData`, 在开发Plutarch期间可能必须这样做, 您可以在[Plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)找到您需要了解的所有内容。

## `PConstant`和`PLift`

TODO

# 低级别示例

## 手动从`ScriptContext`中提取`txInfoInputs`(UNTYPED)

这里快速回顾一下 `ScriptContext`的样子:

```haskell
data ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
```

我们对`txInfoInputs`感兴趣，它的类型为 `TxInInfo`。它是`TxInfo`中的第一个字段。如果您已经阅读过[Working with `BuiltinData`](#working-with-builtindatadatapdata) - 您知道`ScriptContext`会转换为`Data`值，类似于:

```haskell
Constr 0 [PlutusTx.toData txInfo, PlutusTx.toData txPurpose]
```

其中`txInfo`和`txPurpose`分别是`TxInfo`和`ScriptPurpose`类型的值。

我们对第一个字段很感兴趣。这很简单，我们依次执行以下操作:

- `pasConstr` - 产生一个`PBuiltinPair PInteger (PBuiltinList PData)`. 我们知道构造函数id是`0`。没关系，只有一个构造函数。
- `psndBuiltin` - 产生`PBuiltinList PData`, `pair`中的第二个元素。这些是`ScriptContext`中的字段。
- `phead` - 产生`PData`, 第一个字段.我们知道这是我们的`TxInfo`.

将所有这些结合起来后会给你:

```haskell
import Plutarch.Prelude
import Plutarch.Builtin

f :: Term s (PData :--> PData)
f = plam $ \x -> phead #$ psndBuiltin #$ pasConstr # x
```

如果您使用模拟上下文值对其进行测试，它确实有效:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval
import qualified PlutusTx

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      [ TxInInfo
          (TxOutRef "" 1)
          (TxOut (Address (PubKeyCredential "0123") Nothing) mempty Nothing)
      ]
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

>  f `evalWithArgsT` [PlutusTx.toData mockCtx]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf data (Constr 0 [List [Constr 0 [Constr 0 [Constr 0 [B ""],I 1],Constr 0 [Constr 0 [Constr 0 [B "\SOH#"],Constr 1 []],Map [],Constr 1 []]]],List [],Map [],Map [],List [],List [],Constr 0 [Constr 0 [Constr 1 [I 1],Constr 1 []],Constr 0 [Constr 1 [I 2],Constr 1 []]],List [],List [],Constr 0 [B ""]])))))
```

> 另外：你可以在上面找到`evalWithArgsT`的定义 - [编译和运行](./GUIDE.md#compiling-and-running)。

但我们还没有完成! 我们想要`txInfoInputs`。您可能已经注意到它在上述输出中的确切位置。看到那个`List ...`了吗？在最外面的`Constr`的字段里面？那就是我们的`txInfoInputs`!

> 旁白：回想一下，`List`数据值只是列表的包装器。还要记住，`Constr`值中的字段必须都是`Data`类型。因此，您的任何列表字段都会转换为`List`数据。请记住不要将这些与内置列表(`PBuiltinList`)混淆！`pheadBuiltin`之类的函数不适用于 `List`数据值。

要从这里获取 `txInfoInputs`，我们依次执行以下操作:

- `pasConstr` - 解压`TxInfo`。只有一个构造函数，`TxInfo` - 我们不关心这个。我们需要这些字段。
- `psndBuiltin` - 提取`pair`的第二个成员，`TxInfo`的字段。
- `phead` - 提取列表的第一个元素。这是我们的字段，`txInfoInputs`。
- (optional) `pasList` - 从`List`数据值中取出内置列表。

就是这样！把它们放在一起:

```haskell
f :: Term s (PData :--> PBuiltinList PData)
f = plam $ \x ->
  let txInfo = phead #$ psndBuiltin #$ pasConstr # x
  in pasList #$ phead #$ psndBuiltin #$ pasConstr # txInfo
```

在相同的`mockCtx`上尝试会产生:

```haskell
> f `evalWithArgsT` [PlutusTx.toData mockCtx]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf list (data) [Constr 0 [Constr 0 [Constr 0 [B ""],I 1],Constr 0 [Constr 0 [Constr 0 [B "\SOH#"],Constr 1 []],Map [],Constr 1 []]]]))))
```

去掉一些样板文件，这就是值的样子:

```haskell
Some
  (ValueOf list (data)
    [Constr 0
        [Constr 0 [Constr 0 [B ""],I 1],Constr 0 [Constr 0 [Constr 0 [B "\SOH#"],Constr 1 []],Map [],Constr 1 []]]
    ]
  )
```

在这个例子中，`txInfoInputs` 中只有一个元素，它就在那里。当然，这个列表的元素类型`TxInInfo` 也被转换为具有更多字段的`Constr`数据。这就是你在上面看到的。

# 有用链接

- [Builtin lists](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md)
- [Builtin pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md)
- [Builtin functions](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-functions.md)
- [Builtin data](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)
- [Plutus builtin functions and types](https://playground.plutus.iohkdev.io/doc/haddock//plutus-tx/html/PlutusTx-Builtins-Internal.html)
- [Plutus Core builtin function identifiers, aka `DefaultFun`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultFun)
- [Plutus Core types, aka `DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni)
