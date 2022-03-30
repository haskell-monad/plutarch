# Data encoding and Scott encoding

在Plutus Core中，实际上有两种(冲突的)方式来表示`non-trivial`(有意义的)ADTs: [`Constr`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data)`data`编码，或`Scott`编码。对于你的`non-trivial`(有意义的)类型，您应该只使用这些表示中的一种。

> 旁白: 什么是`trivial`(琐碎的)类型？`non-data`内置类型！`PInteger`、`PByteString`、`PBuiltinList`、`PBuiltinPair`和`PMap`(实际上只是内置`pairs`(对)的内置列表). 重要的是要注意[`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data)(`Constr`或其他)也是内置类型。

## Data encoding

`Constr` data本质上是`sum-of-products`表示。但是，它只能包含其他`Data`值(不一定只是`Constr`数据，可以是`I`数据、`B`数据等)作为它的字段。众所周知，Plutus Core缺乏使用这种编码表示函数的能力，因此`Constr`编码的值不能包含函数。

> 注意: 您可以在[plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)找到更多关于`Data`/`BuiltinData`的详细信息。

话虽如此，`Data`编码在链上无处不在。它是分类帐API类型使用的编码，它是可以传递给链上脚本等的参数类型。因此，您的`datums`和`redeemers`必须使用数据编码.

## Scott encoding

在相反(冲突的)的一端，是`Scott`编码。[互联网](https://crypto.stanford.edu/~blynn/compiler/scott.html)可以比我更好地解释`Scott`编码方式。但无论如何，我都会用一个例子来演示`Scott`编码。

首先，Scott编码有什么好处？好吧，它可以包含函数！但是，您不能在例如您的`datums`和`redeemers`中使用Scott编码的类型。

简而言之，Scott编码是一种用函数表示数据的方法。`Maybe a`的Scott编码表示将是:

```hs
(a -> b) -> b -> b
```

`Just 42`, 例如，将表示为这个函数:

```hs
\f _ -> f 42
```

而`Nothing`将被表示为这个函数:

```hs
\_ n -> n
```

我们涵盖了构造。使用/解构呢？这也很简单。假设你有一个函数，`foo :: Maybe Integer -> Integer`，它接收一个Scott编码的`Maybe Integer`，并将`42`添加到它的`Just`值。如果它是`Nothing`，它只返回`0`.

```hs
{-# LANGUAGE RankNTypes #-}

import Prelude (Integer, (+))

type Maybe a = forall b. (a -> b) -> b -> b

just :: a -> Maybe a
just x = \f _ -> f x

nothing :: Maybe a
nothing = \_ n -> n

foo :: Maybe Integer -> Integer
foo mb = mb (\x -> x + 42) 0
```

How does that work? Recall that `mb` is really just a function. Here's how the application of `f` would work:
这是如何运作的？回想一下，`mb`实际上只是一个函数。以下是`f`的应用程序的工作原理:

```hs
foo (just 1)
foo (\f _ -> f 1)
(\f _ -> f 1) (\x -> x + 42) 0
(\x -> x + 42) 1
43
```

```hs
foo nothing
foo (\_ n -> n)
(\_ n -> n) (\x -> x + 42) 0
0
```

整洁的！

这与`PMaybe`的实现中遵循的方法相同。查看它的[PlutusType impl](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)！
