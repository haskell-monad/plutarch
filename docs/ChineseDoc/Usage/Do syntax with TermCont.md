# Do syntax with `TermCont`

像`pmatch`、`plet`和`pletFields`这样的延续函数并不是最方便的，不是吗？幸运的是，`TermCont`使它更易于使用。`TermCont`是熟悉的[`Cont`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Cont.html)monad，专门用于Plutarch terms。

`TermCont @b s a`本质上代表`(a -> Term s b) -> Term s b`。 `a`是延续的输入，`Term s b` 是输出。请注意类型应用 - `b`必须首先通过另一个绑定进入范围。

考虑一下这个片段:

```hs
import Plutarch.Api.V1.Contexts
import Plutarch.Prelude

test :: Term s (PScriptPurpose :--> PUnit)
test = plam $ \x -> pmatch x $ \case
  PSpending _ -> ptrace "matched spending script purpose" $ pconstant ()
  _ -> ptraceError "pattern match failure"
```

太难看的！[`pmatch`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)将延续作为其第二个参数。我们可以让它更符合人体工程学吗？

```hs
import Plutarch.Api.Contexts
import Plutarch.Prelude

pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

ptraceC :: Term s PString -> TermCont s ()
ptraceC s = tcont $ \f -> ptrace s (f ())

test :: Term s (PScriptPurpose :--> PUnit)
test = plam $ \x -> unTermCont $ do
  PSpending _ <- pmatchC x
  ptraceC "matched spending script purpose"
  pure $ pconstant ()
```

多么酷啊？您可以在`TermCont` monad上使用常规的`do`语法。所有的延续都被压平了！只需要记住`unTermCont`结果。

此外，这与`Cont` monad 非常相似 —— 它只是在Plutarch级别的terms上运行。这意味着您可以与使用`Cont` monad时使用的实用程序和模式进行比较。下面一个例子:

```hs
import Plutarch.Prelude

-- | 以空列表上的给定值终止，否则继续head和tail。
nonEmpty :: Term s r -> PList a s -> TermCont @r s (Term s a, Term s (PList a))
nonEmpty x0 list = tcont $ \k ->
  case list of
    PSCons x xs -> k (x, xs)
    PSNil -> x0

foo :: Term s (PList PInteger :--> PInteger)
foo = plam $ \l -> unTermCont $ do
  (x, xs) <- nonEmpty 0 =<< tcont (pmatch l)
  pure $ x + plength # xs
```

`foo`将给定列表的第一个元素与其尾部的长度相加。除非列表为空，在这种情况下，它只返回0。它使用带有`do`语法的延续来优雅地利用短路！