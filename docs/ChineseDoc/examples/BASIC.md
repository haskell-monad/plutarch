演示Plutarch用法的基本示例。

> 注意：如果您发现任何错误/有任何本指南无法回答的相关问题，请不要犹豫，立即提出问题。我们的目标是为Plutarch用户提供高质量的文档！

> 另外: 一定要先看看[编译和运行](./../GUIDE.md#compiling-and-running)！

# Fibonacci number at given index

```hs
import Plutarch.Prelude

fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      0
      $ pif
        (n #== 1)
        1
        $ self # (n - 1) + self # (n - 2)
```

来自[示例](https://github.com/Plutonomicon/plutarch/tree/master/plutarch-test).

执行:

```hs
> evalT $ fib # 2
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf integer 2))))
```
