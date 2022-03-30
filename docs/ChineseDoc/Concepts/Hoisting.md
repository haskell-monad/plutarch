# Hoisting, metaprogramming, and fundamentals

提升、元编程和基础知识

Plutarch有一个两阶段的编译过程。首先GHC编译我们的代码，然后我们的代码生成Plutus脚本的AST，然后使用`compile`对其进行序列化。

需要注意的重要一点是，当您有如下定义时:

```haskell
x :: Term s PInteger
x = something complex
```

任何使用`x`都会内联`x`的完整定义。`x + x`将在AST中复制`something complex`。为避免这种情况，您应该[使用`plet`以避免重复工作](./../Tricks/Don't%20duplicate%20work.md)。请注意，这是经过严格评估的，因此并不总是最好的解决方案。

There is however still a problem: what about top-level functions like `fib`, `sum`, `filter`, and such? We can use `plet` to avoid duplicating the definition, but this is error-prone. To do this perfectly means that each function that generates part of the AST would need to have access to the `plet`'ed definitions, meaning that we'd likely have to put it into a record or typeclass.
然而还有一个问题: 像`fib`、`sum`、`filter`之类的顶级函数呢？我们可以使用`plet`来避免重复定义，但这很容易出错。要完美地做到这一点，意味着生成部分AST的每个函数都需要访问`plet`ed 定义，这意味着我们可能必须将其放入记录或类型类中。

为了解决这个问题，Plutarch支持`hoisting`(提升)。`Hoisting`仅适用于`closed terms`，即不引用任何自由变量的terms(通过`plam`引入)。

`Hoisted terms`本质上被移动到顶级`plet`，即它本质上是常见的子表达式消除。请注意，因此，你的`hoisted term`也经过严格评估，这意味着您不应该`hoist`非惰性的复杂计算(使用[`pdelay`](./../Introduction/Delay%20and%20Force.md)以避免这种情况).

通常，您应该在每个顶级函数上使用`phoistAcyclic`:

```hs
foo = phoistAcyclic $ plam $ \x -> <something complex>
```

只要您要提升的Plutarch lambda没有[自由变量](https://wiki.haskell.org/Free_variable)(作为Plutarch terms)，您就可以`hoist`它！

## Hoisting Operators

为方便起见，您通常会希望使用运算符 - 这必须是`Haskell`级别的函数。`+`、`-`、`#==`等等都是这种情况。

选择方便而不是效率是困难的，但是如果您注意到您的运算符使用复杂的逻辑并且可能最终创建大的terms - 您可以将逻辑简单地分解为Plutarch级别的函数，将其`hoist`(提升)，然后简单地在运算符中应用该函数。


考虑 "boolean or":

```hs
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pif x (pconstant True) $ pif y (pconstant True) $ pconstant False
```

您可以将大部分逻辑分解为Plutarch级别的函数，并将其应用到运算符定义中:

```hs
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

por :: Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x y -> pif' # x # pdelay (pconstant True) # y
```

一般来说，模式是这样的:

```hs
(<//>) :: Term s x -> Term s y -> Term s z
x <//> y = f # x # y

f :: Term s (x :--> y :--> z)
f = phoistAcyclic $ plam $ \x y -> <complex computation>
```

(或者，简单地内联):

```hs
(<//>) :: Term s x -> Term s y -> Term s z
x <//> y = (\f -> f # x # y) $ phoistAcyclic $ plam $ \x y -> <complex computation>
```

> 注意: 您甚至不需要导出Plutarch级别函数或任何东西！您可以简单地将复杂的逻辑分解为一个"`hoisted`(提升的)内部Plutarch函数"，一切都会正常工作！