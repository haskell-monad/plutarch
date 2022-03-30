# Pattern matching constant `Term`s with `pmatch`.

我们已经展示了如何从种类为`PType`(即`pcon . PJust`)的类型的数据构造函数中构造`Term`。接下来，很自然，我们可能希望使用已知的`PType`标记(即类型为`Term s (PMaybe a)` 的值)对`Term`进行模式匹配，以生成另一个`Term`(即，取决于关于该值是否匹配`PJust _`或`Nothing`)

我们需要的函数是`PMatch`类型类的方法。暂且忽略实现的细节，只看类型:

```hs
pmatch :: forall (a :: PType) (s :: S) (b :: PType).
    PMatch a =>               {- 我们对类型`a`有两个约束:
                                  它有一个`PMatch`实例. -}
    Term s a ->               -- 给定一个用`a`标记的`Term`...
    (a s -> Term s b) ->      -- ...和一个从`a s`到`Term s b`的函数...
    Term s b                  -- ...产生一个`Term s b`
```

第二个参数的注释值得关注; 第二个参数的类型显示为`(a s -> Term s b)`。首先，回想一下，`a`被声明为具有`PType`种类，而`PType`是`S -> Type`的种类同义词。因此，由于`s`具有种类`S`，我们拥有具有种类`Type`的`a s`。也就是说，它是常规的Haskell类型。

实际上，这意味着`pmatch`与`Term s a`评估结果的可能值相匹配 —— 具体来说，它匹配具有种类`PType`的类型的值 - 并相应地进行分支。
`pmatch`的第二个参数是一个延续; 一旦`pmatch`完成它的工作，它决定了程序如何继续。

我们已经介绍了一种适合分支的种类为`PType`的类型: `PMaybe`。 这是一个例子：

```hs
{- | 这个函数接受一个Haskell级别的`PMaybe`值(特别是，它不是`Term`)，
     并根据`PMaybe`的数据构造函数上的Haskell级别模式匹配返回一个`Term`.
-}
continuation :: PMaybe a s -> Term s PBool
continuation x = case x of
  PJust _ -> pconstant True
  PNothing -> pconstant False

{- | A Haskell-level `isJust` on Plutarch `Term`s. `pmatch` can match on
     the possibilities of `PJust _` or `PNothing` being the result of an evaluated
     `Term`.
     Plutarch `Term`上的Haskell级别的`isJust`。`pmatch`可以匹配`PJust _`或`PNthing`作为评估`Term`的结果的可能性。
-}
hisJust :: Term s (PMaybe a) -> Term s PBool
hisJust x = pmatch x continuation

-- | A Plutarch-level `isJust`
pisJust :: Term s (PMaybe a :--> PBool)
pisJust = plam hisJust
```

读者应该注意，这不是处理模式匹配的最符合人体工程学的方法(Plutarch提供了两个版本的`do`语法)，但它是更符合人体工程学的方法在幕后工作的方式。