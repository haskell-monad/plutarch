Plutarch指南是您快速了解Plutarch的一站式资源！

> 注意: 如果您发现任何错误/有任何本指南无法回答的相关问题，请不要犹豫，立刻提出问题。我们的目标是为Plutarch用户提供高质量的文档！

<details>
<summary> 目录 </summary>

- [概述](#概述)
  - [编译运行](#编译运行)
  - [简介和基础语法](#简介和基础语法)
  - [实际运用](#实际运用)
  - [概念](#概念)
  - [类型类](#类型类)
  - [使用类型](#使用类型)
- [示例](#示例)
- [经验法则、提示和技巧](#经验法则提示和技巧)
- [常见问题和故障排除](#常见问题和故障排除)
- [有用的链接](#有用的链接)

</details>

> 旁白: 对细节不感兴趣？直接跳到[示例](#示例)!

# 概述

## 编译运行

- [通用扩展和GHC选项](./Run.md#通用扩展和GHC选项)
- [评估](./Run.md#评估)

## 简介和基础语法

[简介部分](./简介.md)用于介绍Plutarch的基本概念和核心句法结构。它将有助于建立Plutarch的心智模型，但不足以编写可用于生产的代码。

- [概述](./简介.md#概述)
- [无类型Plutus Core (UPLC)](./Introduction/Untyped%20Plutus%20Core.md)
- [Plutarch类型](./Introduction/Plutarch%20Types.md)
- [Plutarch `Term`s](./Introduction/Plutarch%20Terms.md)
  - [Plutarch Constant `Term`s](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md)
    - [Static building of constant `Term`s with `pconstant`](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md#static-building-of-constant-terms-with-pconstant)
    - [Dynamic building of constant `Term`s with `pcon`](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md#dynamic-building-of-constant-terms-with-pcon)
    - [重载字面量](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md#overloaded-literals)
    - [助手函数](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md#helper-functions)
  - [Lambdas; Plutarch-level Function `Term`s.](./Introduction/Plutarch%20Terms/Plutarch%20Lambdas.md#lambdas-plutarch-level-function-terms)
    - [Function Application](./Introduction/Plutarch%20Terms/Plutarch%20Lambdas.md#function-application)
- [Pattern matching constant `Term`s with `pmatch`.](./Introduction/Pattern%20matching.md)
- [严格和懒惰； 延迟的Terms和强制](./Introduction/Delay%20and%20Force.md)
- [参考](./Introduction.md#references)

## 实际运用

[用法部分](./Usage.md)填补了前面留下的空白。它展示了使Plutarch更易于使用的技术。

- [条件语句](./Usage/Conditionals.md)
- [递归](./Usage/Recursion.md)
- [Do syntax with `TermCont`](./Usage/Do%20syntax%20with%20TermCont.md)
- [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](./Usage/Do%20syntax%20with%20QualifiedDo.md)
- [为newtypes派生类型类](./Usage/Deriving%20for%20newtypes.md)
- [使用generics(泛型)派生类型类](./Usage/Deriving%20with%20generics.md)
- [`plet`避免重复工作](./Usage/Avoid%20work%20duplication%20using%20plet.md)
- [跟踪](./Usage/Tracing.md)
- [引发错误](./Usage/Raising%20errors.md)
- [不安全的函数](./Usage/Unsafe%20functions.md)

## 概念

[概念部分](./Concepts.md)详细介绍了其他概念。

- [Hoisting, 元编程, 基本原理](./Concepts/Hoisting.md)
  - [Hoisting Operators](./Concepts/Hoisting.md#hoisting-operators)
- [`s`是什么?](./Concepts/What%20is%20the%20S.md#what-is-the-s)
- [Data编码和Scott编码](./Concepts/Data%20and%20Scott%20encoding.md)
  - [Data编码](./Concepts/Data%20and%20Scott%20encoding.md#data-encoding)
  - [Scott编码](./Concepts/Data%20and%20Scott%20encoding.md#scott-encoding)
- [Plutarch类型的Haskell同义词](./Concepts/Haskell%20Synonym.md)

## 类型类

[Typeclasses部分](./Typeclasses.md)讨论了与Plutarch相关的主要类型类。

- [`PEq` & `POrd`](./Typeclasses/PEq%20and%20POrd.md)
- [`PIntegral`](./Typeclasses/PIntegral.md)
- [`PIsData`](./Typeclasses/PIsData.md)
- [`PlutusType`, `PCon`, and `PMatch`](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)
  - [为你自己的类型(`Scott`编码)实现`PlutusType`](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding)
  - [为你自己的类型(`Data`编码)实现`PlutusType`](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-data-encoding)
  - [为你自己的类型(`newtype`)实现`PlutusType`](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-newtype)
- [`PConstant` & `PLift`](./Typeclasses/PConstant%20and%20PLift.md)
  - [实现`PConstant` & `PLift`](./Typeclasses/PConstant%20and%20PLift.md#implementing-pconstant--plift)
  - [为具有类型变量的类型(generic泛型类型)实现`PConstant`和`PLift`](./Typeclasses/PConstant%20and%20PLift.md#implementing-pconstant--plift-for-types-with-type-variables-generic-types)
- [`PListLike`](./Typeclasses/PListLike.md)
- [`PIsDataRepr` & `PDataFields`](./Typeclasses/PIsDataRepr%20and%20PDataFields.md)
  - [关于提取字段的一切](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#all-about-extracting-fields)
    - [`OverloadedRecordDot`的替代品](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#alternatives-to-overloadedrecorddot)
  - [关于构造data values的一切](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#all-about-constructing-data-values)
  - [Implementing `PIsDataRepr` and friends](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)

## 使用类型

[类型部分](./Types.md)讨论了Plutarch的核心类型。

- [`PInteger`](./Types/PInteger.md)
- [`PBool`](./Types/PBool.md)
- [`PString`](./Types/PString.md)
- [`PByteString`](./Types/PByteString.md)
- [`PUnit`](./Types/PUnit.md)
- [`PBuiltinList`](./Types/PBuiltinList.md)
- [`PList`](./Types/PList.md)
- [`PBuiltinPair`](./Types/PBuiltinPair.md)
- [`PTuple`](./Types/PTuple.md)
- [`PAsData`](./Types/PAsData.md)
- [`PDataSum` & `PDataRecord`](./Types/PDataSum%20and%20PDataRecord.md)
- [`PRecord`](./Types/PRecord.md)
  - [letrec](./Types/PRecord.md#letrec)
  - [Record Data](./Types/PRecord.md#record-data)
- [`PData`](./Types/PData.md)

# 示例

- [基本示例](./examples/BASIC.md)
- [验证器和铸造策略](./examples/VALIDATOR.md)

另见: [示例](https://github.com/Plutonomicon/plutarch/tree/master/plutarch-test).

# 经验法则、提示和技巧

除了基本用户指南之外，您还可以遵循一些经验法则和一般准则来改善您的Plutarch 体验。[技巧部分](./Tricks.md)讨论了编写高效和高质量Plutarch代码的方法，以及有助于简化Plutarch审计的规则。

- [Plutarch函数是严格的](./Tricks/Plutarch%20functions%20strict.md)
- [不要重复工作](./Tricks/Don't%20duplicate%20work.md)
  - [Where should arguments be `plet`ed?](./Tricks/Don't%20duplicate%20work.md#where-should-arguments-be-pleted)
- [更喜欢Plutarch级别的函数](./Tricks/Prefer%20Plutarch%20functions.md)
- [何时使用Haskell级别的函数？](./Tricks/Using%20Haskell%20level%20functions.md)
- [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](./Tricks/Difference%20between%20pcon%20and%20pconstant.md)
- [让Haskell级别的函数负责评估](./Tricks/Responsibility%20of%20evaluation%20in%20Haskell%20functions.md)
- [The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`](./Tricks/makeIsDataIndexed,%20Haskell%20ADTs,%20and%20PIsDataRepr.md)
- [尽可能选择静态构建常量](./Tricks/Prefer%20statically%20building%20constants.md)
- [弄清楚Plutarch类型的表示](./Tricks/Representation%20of%20Plutarch%20type.md)
- [Prefer pattern matching on the result of `pmatch` immediately](./Tricks/Prefer%20matching%20on%20pmatch%20result%20immediately.md)

# 常见问题和故障排除

由于`Plutarch`的高度抽象性及其对高级类型级别概念的使用，您可能会遇到不熟悉的错误。
别担心，这里的指南可以为您提供帮助！

- [No instance for `PUnsafeLiftDecl a`](./Troubleshooting.md#no-instance-for-punsafeliftdecl-a)
- [Couldn't match representation of type: ... arising from the 'deriving' clause](./Troubleshooting.md#couldnt-match-representation-of-type--arising-from-the-deriving-clause)
- [Infinite loop / Infinite AST](./Troubleshooting.md#infinite-loop--infinite-ast)
- [Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `hrecField`, or `pletFields`)](./Troubleshooting.md#couldnt-match-type-plutarchdatareprinternalpunlabel--arising-from-a-use-of-pfield-or-hrecfield-or-pletfields)
- [Expected a type, but "fieldName" has kind `GHC.Types.Symbol`](./Troubleshooting.md#expected-a-type-but-fieldname-has-kind-ghctypessymbol)
- [Lifting `PAsData`](./Troubleshooting.md#lifting-pasdata)
- [Couldn't match type `PLifted (PConstanted Foo)` with `Foo`](./Troubleshooting.md#couldnt-match-type-plifted-pconstanted-foo-with-foo)
- [Type match errors when using `pfield`/`hrecField` (or `OverloadedRecordDot` to access field)](./Troubleshooting.md#type-match-errors-when-using-pfieldhrecfield-or-overloadedrecorddot-to-access-field)

# 有用的链接

- [Plutonomicon](https://github.com/Plutonomicon/plutonomicon)
