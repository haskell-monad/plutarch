# Lambdas; Plutarch-level Function `Term`s.

Lambda是Plutarch `Term`s的第二种形式。Lambda terms在类型级别由中缀类型构造函数`:-->`表示；`Term s (a :--> b)`类型的值计算为一个函数，该函数接受`a`类型的值并产生`b`类型的值。

您可以通过将`plam`函数应用于适用于Plutarch terms的Haskell级函数来创建Plutarch lambda `Term`。`plam`的真正类型本身对Plutarch的最终用户来说并不重要，但应该将其视为:

```hs
plam :: (Term s a -> Term s b) -> Term s (a :--> b)
```

要将恒等函数创建为Plutarch lambda，我们将使用:

```hs
-- | Haskell级别的`id`函数专用于`Term s a`类型
termId :: Term s a -> Term s a
termId x = x

-- | Plutarch-level `id` lambda
pid :: Term s (a :--> a)
pid = plam x

-- | 相当于:
pid' :: Term s (a :--> a)
pid' = plam $ \x -> x
```

注意类型。Plutarch lambda `Term`使用`:-->`中缀运算符来编码函数类型。所以在上面的例子中，`pid`是一个Plutarch级别的函数，它接受一个类型`a`并返回相同的类型。正如人们所期望的那样，`:-->` 是右结合的，and things curry like a charm.

猜猜这个Plutarch级别函数的作用:

```hs
f :: Term s (PInteger :--> PString :--> a :--> a)
```

它接受一个整数、一个字符串和一个类型`a`并返回相同的类型`a`。请注意，这些类型都具有种类`PType`。这意味着当面临填补空白时:

```hs
f :: Term s (PInteger :--> PString :--> a :--> a)
f = plam $ \???
```

我们知道`plam`的参数是一个Haskell函数`g`，其类型为`Term s PInteger -> Term s PString -> Term s a -> Term s a`。

## Function Application

一旦我们使用`plam`构造了一个Plutarch lambda `Term`，除非我们将其应用于参数，否则它将毫无用处。Plutarch提供了两个运算符来执行此操作:

```hs
{- |
  高优先级中缀函数应用，用于函数并列。 例如:
  >>> f # x # y
  概念上: f x y
-}
(#) :: Term s (a :--> b) -> Term s a -> Term s b
infixl 8 #

{- |
  低优先级中缀函数应用程序，与`$`一样使用，与`#`结合使用. 例如:
  >>> f # x #$ g # y # z
  概念上: f x (g y z)
-}
(#$) :: Term s (a :--> b) -> Term s a -> Term s b
infixr 0 #$
```

The types of each operator match our intuition. Applying a lambda `Term` to a `Term` (tagged with the `PType` of the domain of the lambda) produces a `Term` (tagged with the `PType` of the codomain.).
每个运算符的类型都符合我们的直觉。将lambda `Term`应用于`Term`(用lambda域的`PType`标记)会产生一个`Term`(用codomain的`PType`标记)。