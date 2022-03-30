> 注意: 如果您发现任何错误/有任何本指南无法回答的相关问题，请不要犹豫，立即提出问题。我们的目标是为Plutarch用户提供高质量的文档！

<details>
<summary> 目录 </summary>

- [No instance for `PUnsafeLiftDecl a`](#no-instance-for-punsafeliftdecl-a)
- [Couldn't match representation of type: ... arising from the 'deriving' clause](#couldnt-match-representation-of-type--arising-from-the-deriving-clause)
- [Infinite loop / Infinite AST](#infinite-loop--infinite-ast)
- [Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `hrecField`, or `pletFields`)](#couldnt-match-type-plutarchdatareprinternalpunlabel--arising-from-a-use-of-pfield-or-hrecfield-or-pletfields)
- [Expected a type, but "fieldName" has kind `GHC.Types.Symbol`](#expected-a-type-but-fieldname-has-kind-ghctypessymbol)
- [Lifting `PAsData`](#lifting-pasdata)
- [Couldn't match type `PLifted (PConstanted Foo)` with `Foo`](#couldnt-match-type-plifted-pconstanted-foo-with-foo)
- [Type match errors when using `pfield`/`hrecField` (or `OverloadedRecordDot` to access field)](#type-match-errors-when-using-pfieldhrecfield-or-overloadedrecorddot-to-access-field)

</details>

# No instance for `PUnsafeLiftDecl a`

您应该将`PLift a`添加到上下文中！`PLift`只是`PUnsafeLiftDecl`的同义词。

# Couldn't match representation of type: ... arising from the 'deriving' clause

如果您在使用Plutarch提供的机制派生类型类时遇到这些错误(例如，泛型派生、通过`PIsDataReprInstances`、`DerivePConstantViaData`派生等) - 这意味着您缺少构造函数导入。

如果您在使用`DerivingVia`时遇到此问题，请确保您已导入您要通过派生的类型的构造函数。

如果您在使用泛型派生时遇到此问题，请确保您已导入`I`构造函数(或任何其他相关构造函数):

```hs
import Generics.SOP (Generic, I (I))
```

# Infinite loop / Infinite AST

无限循环/无限 AST

虽然可能不是很明显，但以下内容在 Plutarch 中是`no-go`(不允许)的:

```haskell
f :: Term s (PInteger :--> PInteger)
f = phoistAcyclic $ plam $ \n ->
  pif (n #== 0)
    0
    $ n + f # (n - 1)
```

这里的问题是AST是无限大的。Plutarch将尝试遍历此AST，并且在此过程中不会终止，因为它没有尽头。在这些情况下，请考虑使用`pfix`。


相关问题: [#19](https://github.com/Plutonomicon/plutarch/issues/19)

# Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `hrecField`, or `pletFields`)

像上面这样使用`pfield`/`hrecField`/`pletFields`时，您可能会遇到一些奇怪的错误。不要害怕！这只是意味着您使用的类型应用程序不正确。具体来说，类型应用程序命名了一个不存在的字段。重新检查您在类型应用程序中使用的字段名称字符串是否有错别字！


# Expected a type, but "fieldName" has kind `GHC.Types.Symbol`

这只是意味着没有正确提升类型应用程序的参数。很可能是因为使用了`pletFields`。
在`pfield`和`hrecField`的情况下，类型应用程序的参数应该具有种类`Symbol`。在这种情况下，一个表示字段名称的简单字符串字面量应该可以工作。
在 `pletFields` 的情况下，类型应用程序的参数应该具有种类`[Symbol]` - 具有种类`Symbol`的类型的类型级别列表(`a type level list of types with kind Symbol`)。
当您在此处使用单个列表时，例如`["foo"]` - 它实际上被解析为常规列表(例如`[a]`)。当然，常规列表具有种类`Type`。

你需要做的就是在列表前面放一个`'`(引号)，就像这样-`@'["far"]`。这会将 `[a]` 提升到类型级别。

# Lifting `PAsData`

不要试图去`lift`(提升)一个`PAsData` term! 它是故意屏蔽和部分屏蔽的。`PAsData`的`PLift`实例仅用于使某些重要功能正常工作。但是，如果使用实例方法，则只会出现`error`。相反，您应该使用`pforgetData`和`plift`，或者使用`pfromData`和`plift`从`Term s (PAsData a)`中提取`Term s a`！


# Couldn't match type `PLifted (PConstanted Foo)` with `Foo`

`PLifted (PConstanted h)` 应该总是`h` - 对吧? 那这是什么?

孤儿实例！具体来说，为了使这些类型族应用程序能够完全计算(并产生`h`)，您需要范围内的`h`的`PConstant`实例，以及相应Plutarch类型的`PLift`实例。回想一下，这里的`h`是一个Haskell类型 —— 它对应的`PConstant`实例可能是一个你没有导入的孤立实例。

这经常发生在Plutarch分类帐API类型中。如果你没有导入`Plutarch.Api.V1.Contexts`(或其他导入它的模块)，并且你在`ScriptContext`上使用了`pconstant` - 你会得到这样的错误。
`ScriptContext`的`PConstant`实例尚未导入 - 所以GHC不知道`PConstant ScriptContext`是什么！


相关问题: [#252](https://github.com/Plutonomicon/plutarch/issues/252)

# Type match errors when using `pfield`/`hrecField` (or `OverloadedRecordDot` to access field)

提取字段时，您可能会收到无意义的"Couldn't match type"错误。这与GHC错误地推断返回类型有关。字段提取意味着它的返回类型是多态的，因为它可能返回一个`Term s (PAsData p)` term，或者只是一个`Term s p`(`automatic`(自动的/无意识的)`pfromData`).不幸的是，有时这种多态性使GHC更难推断类型。

您可以通过在`pfield`或`hrecField`(或用于字段访问`OverloadedRecordDot`)的结果上提供显式类型注释来解决此问题。否则，您也可以在结果上显式使用`pfromData`。

相关问题: [#275](https://github.com/Plutonomicon/plutarch/issues/275)
