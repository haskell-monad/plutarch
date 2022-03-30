# Do syntax with `QualifiedDo` and `Plutarch.Monadic`

还有另一种使用`do`语法的方法。虽然这个不使用合法的单子。相反，它使用 `QualifiedDo` - 因此需要 GHC 9。

`Plutarch.Monadic`模块导出适合与`QualifiedDo`一起使用的`>>=`、`>>`和`fail`函数。

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}

import Plutarch.Api.Contexts
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

f :: Term s (PScriptPurpose :--> PUnit)
f = plam $ \x -> P.do
  PSpending _ <- pmatch x
  ptrace "matched spending script purpose"
  pconstant ()
```

本质上，`P.do { x; y }` 简单地转换为 `x y`；其中`x :: a -> Term s b`和`y :: a`。

同样，`P.do { y <- x; z }` 转换为 `x $ \case { y -> z; _ -> ptraceError <msg> }`; 其中`x :: (a -> Term s b) -> Term s b`、`y :: a`和`z :: Term s b`。当然，如果`y`是一个完全穷举模式匹配(例如单数构造函数)，则根本不会生成额外的`_ -> ptraceError <msg>`情况，你只会得到 `x $ \y -> z`。

最后，`P.do { x }` 就是 `x`。