# Plutarch Constant `Term`s

评估时，一个常量Plutarch`Term`将始终产生相同的结果。有几种方法可以构建常量 `Term`：

- 当我们在编译时知道值时，从具体的Haskell值静态构建常量`Term`。
- 从Haskell值动态构建常量`Term`，即当产生的常量依赖于动态值时。
- Overloaded字面量语法
- 助手函数

## Static building of constant `Term`s with `pconstant`

如果我们在编译时知道常量`Term`的期望值，我们可以直接从[Haskell synonyms](./../../Concepts/Haskell%20Synonym.md)构建`Term`。这样做的函数是`pconstant`。

以这种方式构造常量使用[`PConstant`/`PLift`](./../../Typeclasses/PConstant%20and%20PLift.md)类型类。这些类型类公开了以下[关联类型族](https://wiki.haskell.org/GHC/Type_families#An_associated_type_synonym_example):

```hs
type PLifted :: PType -> Type

type PConstanted :: Type -> PType
```

`pconstant`接受一个参数: 带有`PConstant`/`PLift`实例的常规Haskell类型，并产生一个用相应Plutarch类型标记的Plutarch term。

Plutarch类型与其Haskell同义词之间的关系是由类型族建立的。对于任何Haskell类型 `h`，`PConstanted h`是对应的Plutarch类型。同样，对于任何Plutarch类型`p`，`PLifted p`对应于Haskell的同义词。

合法的实例应遵守以下不变量:

```hs
PLifted (PConstanted h) ~ h
PConstanted (PLifted p) ~ p
```

例如:

```hs
import Plutarch.Prelude

-- | 一个Plutarch级别的boolean,在本例中它的值是"True".
x :: Term s PBool
x = pconstant True
```

The familiar `Bool` has a `PConstant` instance and it corresponds to `PBool` (which has a `PLift` instance). Therefore `PLifted PBool ~ Bool` and `PConstanted Bool ~ PBool`.
熟悉的`Bool`有一个`PConstant`实例，它对应于`PBool`(它有一个`PLift`实例)。因此`PLifted PBool ~ Bool`和`PConstanted Bool ~ PBool`.

您还可以使用`pconstantData`直接创建[`PAsData`](./../../Types/PAsData.md)term:

```hs
import Plutarch.Prelude

-- | 一个Plutarch级别的boolean被编码为`Data`.
x :: Term s (PAsData PBool)
x = pconstantData True
```

## Dynamic building of constant `Term`s with `pcon`

有时，我们希望将其视为常量Term的值在编译时是未知的。为了解释当我们只能在运行时确定值时如何构造常量的，我们将检查`PMaybe` Plutarch类型。它可以起到与Haskell中的`Maybe`类型相同的目的: 表示计算可能无法产生合理结果的情况。

`PMaybe`有以下定义:

```hs
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
```

和以下`kind`(种类):

```hs
ghci> :k PMaybe
PMaybe :: PType -> S -> Type
```

让我们剖析一下这意味着什么。

- `PMaybe`从一个`PType`构建`PType`; 给定一个`PType`，我们可以用`PMaybe a`类型标记一个计算，以表明它的返回值在语义上应该是`Just a`或`Nothing`。这样的标记看起来像一个类型为`Term s (PMaybe a)`的值。
- `PJust`和`PNthing`是数据构造函数。它们不是标签。`PJust :: Term s a -> PMaybe (a :: PType) (s :: S)`是表示`Just x`的概念的助手。它包含一个Plutarch term。


现在假设我们想在Plutarch脚本中携带一个常量`Term` ，它可以是`PJust a`或`PNthing`。为此，我们需要一个函数来从`PJust a`(我们可以将其实例化为Haskell值，与`PInteger`不同)到`Term s (PMaybe a)`,这个函数是`pcon`:

```hs
pcon :: a s -> Term s a

-- 例如:

x :: Term s PInteger
x = pconstant 3

justTerm :: Term s (PMaybe PInteger)
justTerm = pcon (PJust x)
```

这些类型值得一些解释.

- 我们现在已经熟悉了`x`的类型；它是一个返回一个值的计算，如果计算成功(在本例中为3)，则该值可以解释为Haskell整数。
- `justTerm`的类型表示使用`PMaybe PInteger`类型标记的计算。

也就是说，如果我们问`justTerm`它在评估时会返回什么，它会回答: "你应该将我给你的值解释为`Nothing`或`Just Integer`。"当然，我们知道结果永远是`Just 3`；但这是声明一个需要一个`Maybe`的函数的一般机制。

`pcon`函数是[`PCon`类型类](./../../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)的方法。

## Overloaded literals

`pconstant`和`pcon`是构建常量的长格式方法。特定的常量Haskell字面量被重载以帮助构造Plutarch常量。我们在下面提供两个示例。

```hs
{-# LANGUAGE OverloadedStrings #-}

import Plutarch.Prelude

-- | 一个Plutarch级别的integer，在本例中它的值是1.
x :: Term s PInteger
x = 1

-- | 一个Plutarch级别的string (这实际上是`Text`), 在本例中它的值是"foobar".
y :: Term s PString
y = "foobar"
```

## Helper functions

最后，Plutarch提供了助手函数来构建某些类型的常量:

```hs
import qualified Data.ByteString as BS
import Plutarch.Prelude

-- | 一个plutarch级别的bytestring. 在本例中它的值是[65].
x :: Term s PByteString
x = phexByteStr "41"
-- ^ 'phexByteStr' interprets a hex string as a bytestring. 0x41 is 65 - of course.
-- ^ 'hexByteStr'将十六进制字符串解释为bytestring, 0x41 是 65.
```
