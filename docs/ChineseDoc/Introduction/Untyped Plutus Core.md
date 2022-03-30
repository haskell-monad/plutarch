# Untyped Plutus Core (UPLC)

Plutarch编译为UPLC。大多数Plutarch最终用户不需要关心UPLC的细节，但简要概述将有助于建立Plutarch工作原理的心智模型。

与Haskell不同，UPLC是一种实现基本lambda演算的低级别无类型语言。因此它只支持在lambda应用程序中串联在一起的少数内置值和函数。UPLC提供的内置类型包括通常的原始类型——整数、字节字符串和字符串、布尔值等等 —— 以及一个特殊的`Data`值，它可以对任意的`Haskell`表示的`sum-of-products`类型进行编码。

虽然UPLC中缺少Haskell类型的语义含义，例如`Maybe Integer`，但它仍然可以通过某些[编码](./../Concepts/Data%20and%20Scott%20encoding.md)在UPLC中表示。前面提到的`Data`编码可用于表示链上组件中的任意类型，例如`Datum`和`Redeemers`。另一方面，Scott Encoding可以另外对函数类型进行编码，但不能在`Datums`或`Redeemers`中使用。关键的想法是，UPLC不会跟踪语义不同的值之间的区别，无论它们的编码如何, 并且不会阻止程序员以无意义的方式对底层表示进行操作。

Plutarch的解决方案是用类型标记编译为UPLC(即Plutarch `Term`s)的脚本。这样做允许Plutarch编译器跟踪和类型检查语义上不同的UPLC值的操作。这些标记由"Plutarch Types"或`PType`种类(`kind`)的类型提供。

为了让Plutarch编译器在任意、语义丰富的Haskell类型和UPLC的无类型值之间架起一座桥梁，有必要将各种信息位与`PType`相关联。一方面，每个`PType`都应该有一些语义、类型级别的丰富性，例如类型类实例(否则，在Haskell中编程就没有什么意义了!)。另一方面，每个`PType`都需要有一个UPLC表示，或者作为内置原始值`Data`，或者作为Scott编码的lambda，以便编译为UPLC。