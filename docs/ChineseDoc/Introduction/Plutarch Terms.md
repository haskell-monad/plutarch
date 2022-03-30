# Plutarch `Term`s

从简单类型的`lambda`演算`terms`意义上说Plutarch `Term`s是terms. 在lambda演算中，我们可以将`terms`构造为"常量"或`lambdas`，terms可以是"开放的"(具有自由变量)或"封闭的"(没有自由变量)。我们编写Plutarch `Term`s来构建越来越复杂的计算。一旦从 `Term`中清除掉所有自由变量(使其成为`Closed Term`)，我们可以使用来自`Plutarch`模块的同名函数对其进行编译:

```hs
-- | Closed term是一个类型同义词
type ClosedTerm (a :: PType) = forall (s :: S). Term s a

-- | Compile 在`closed terms`上运行以生成可用的UPLC脚本
compile :: ClosedTerm a -> Script
```

`Term`s由Haskell值构成，并用`PType`s标记。

- [Plutarch Constant `Term`s](./Plutarch%20Terms/Plutarch%20Constants.md)
  - [Static building of constant `Term`s with `pconstant`](./Plutarch%20Terms/Plutarch%20Constants.md#static-building-of-constant-terms-with-pconstant)
  - [Dynamic building of constant `Term`s with `pcon`](./Plutarch%20Terms/Plutarch%20Constants.md#dynamic-building-of-constant-terms-with-pcon)
  - [Overloaded literals](./Plutarch%20Terms/Plutarch%20Constants.md#overloaded-literals)
  - [Helper functions](./Plutarch%20Terms/Plutarch%20Constants.md#helper-functions)
- [Lambdas; Plutarch-level Function `Term`s.](./Plutarch%20Terms/Plutarch%20Lambdas.md#lambdas-plutarch-level-function-terms)
  - [Function Application](./Plutarch%20Terms/Plutarch%20Lambdas.md#function-application)
