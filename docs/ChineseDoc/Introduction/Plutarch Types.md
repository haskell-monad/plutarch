# Plutarch Types

When this guide uses the term "Plutarch Type" we explicitly talk about a type of _kind_ `PType`. We will refer to  _" types of kind `PType` "_ simply as `PType`s. We explicitly qualify when referring to the _kind_ `PType`.

当本指南使用术语"Plutarch类型"时，我们明确谈论的是`PType`种类(`kind`)的类型。我们将"`PType`种类(`kind`)的类型"简称为`PType`s。我们在引用`PType`种类`kind`时明确限定。

> 初学者请注意: Plutarch使用了一个名为`DataKinds`的语言扩展。这意味着除了`Type`(又名`*`)之外还有其他种类(`kinds`)。如果需要的话，我们可以阅读 \[[3](./../Concepts.md#references)]，以获得对这些概念的入门级介绍。

`PType` is defined as `type PType = S -> Type`; that is, it is a _kind synonym_ for `S -> Type` (where `S` and `Type` are themselves kinds). This synonym is important to keep in mind because when querying the kind of something like `PBool` in, say, GHCi, we will _not_ see `PType` as the kind. Instead, we get

`PType`定义为`type PType = S -> Type`; 也就是说，它是`S -> Type`的种类同义词(其中`S`和`Type`是他自身的种类`kinds`)。记住这个同义词很重要，因为当在GHCi中查询类似`PBool`的种类时，我们不会将`PType`视为种类`kind`。相反，我们得到:

```hs
ghci> :k PBool
PBool :: S -> Type
```

Thus, any time we see the kind `S -> Type`, we should mentally substitute its kind synonym `PType`. We reiterate: types of kind `PType`, should be considered as _tags_ on computation. They do not represent types of values in the same way as standard Haskell types.
因此，任何时候我们看到种类`S -> Type`，我们都应该在心里替换它的种类同义词`PType`。我们重申: `PType`种类的类型应该被视为计算上的标签。它们不像标准的Haskell类型那样表示值的类型。

Haskell中的`Integer`等基本类型有以下种类: `Type`; Plutarch中相应的"基本"种类只是`PType`。Haskell中的更高种类的类型，例如`Maybe`，它的种类为: `Type -> Type`. 在Plutarch中，对应的种类是:

```hs
ghci> :k PMaybe
PMaybe :: PType -> S -> Type
```

Since the kind arrow `->` is right-associative, we first read this as `PMaybe :: PType -> (S -> Type)`; and since we know that that `PType` and `S -> Type` and synonyms, we read this as `PMaybe :: PType -> PType`, which should agree without intuition.

由于种类箭头`->`是右结合的，我们首先将其读作`PMaybe :: PType -> (S -> Type)`; 并且由于我们知道`PType`和`S -> Type`是同义词，我们将其读作`PMaybe :: PType -> PType`，这应该无需直觉就是一致的。

The kind `S -> Type` is mysterious at first, but we recall that `PType`s are _tags_ on (unexecuted) computations indicating their result type. The `S` kind represents the computational context; thus, a `PType` expects to receive a _computational context_ represented by a value `s` whose type has kind `S` that it will tag to produce a `Type`. Note that end-users never instantiate the value `s` with a concrete value; it is simply a type-level mechanism to maintain functional purity.

`S -> Type`种类起初很神秘，但我们记得`PType`是(未执行的)计算上的标签，用于指示其结果类型。`S`种类表示计算上下文; 因此，`PType`期望接收由值`s`表示的计算上下文，其类型具有种类`S`，它将标记以产生`Type`。请注意，最终用户永远不会用具体值实例化值`s`; 它只是一种保持功能纯度的类型级别机制。

The above notion is essential to understanding why not all `PType`s have data constructors; the data constructors are irrelevant, except insofar as they enable the implementation to keep track of Haskell-level and UPLC-level representations. `PInteger` is one such case; it is impossible to construct a constant `y` where `y :: PInteger s`. Other `PType`s, such as `PMaybe`, _do_ have data constructors (specifically `PJust` and `PNothing`), but _still_ do not carry data from the viewpoint of UPLC. A value such as `PNothing` merely facilitates convenient term construction and deconstruction. When `pcon` sees `PNothing`, it knows it should build a UPLC constant that is _morally_ equivalent to the concept of `Nothing :: Maybe a`.

上述概念对于理解为什么不是所有的`PType`都有数据构造函数是必不可少的; 数据构造函数是无关紧要的，除非它们使实现能够跟踪Haskell级和UPLC级表示。`PInteger`就是这样一种情况; 不可能在 `y :: PInteger s`处构造一个常量`y`。其他的`PType`，例如`PMaybe`，确实有数据构造函数(特别是`PJust`和`PNthing`)，但从UPLC的角度来看仍然不携带数据。诸如`PNthing`之类的值仅有助于方便的term构造和解构。当`pcon`看到`PNothing`时，它知道它应该建立一个UPLC常量，这个常量在道德上等同于`Nothing:：Maybe a`的概念。

In general, the concrete UPLC representations are connected to Plutarch types through their `PlutusType` implementation.

一般来说，具体的UPLC表示通过它们的`PlutusType`实现连接到Plutarch类型。

另见: [弄清楚Plutarch类型的表示](./../Tricks/Representation%20of%20Plutarch%20type.md).
