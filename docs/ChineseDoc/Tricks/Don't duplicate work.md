# Don't duplicate work

Haskell绑定在Plutarch编译期间只是"内联"。

考虑一个简单的片段:

```hs
pf :: Term s PInteger
pf =
  let foo = 1 + 2      -- | A Haskell binding.
  in pif
       (foo #== 3)     -- | A.) ...then inline here...
       foo             -- | B.) ...and inline here.
       7
```

使用`printTerm`函数(由顶层`Plutarch`模块提供)，我们可以查看绑定到`foo`的计算。下面的格式是我们自己的; 请注意，在UPLC中变成`(addInteger 1 2)`的`foo`被内联了两次:

```hs
> printTerm pf

(...)

(force
    (force ifThenElse
       (equalsInteger
          (addInteger 1 2)      -- | A.) `foo` appears here...
          3
       )
       (delay (addInteger 1 2)) -- | B.) ...and here
       (delay 7)
    )
)
```

执行两次计算显然是不好的(在这种情况下)，因为它会增加脚本的执行预算。

一种避免这种情况的技术是通过lambda引入一个自由变量，用该变量替换内联表达式(在我们的例子中，`(addInteger 1 2)`)，然后他们将lambda应用于计算的表达式:

```hs
> printTerm pf'

(...)

((\\i0 ->                          -- | A'.) 在这里引入一个lambda...
    force
        (force ifThenElse
            (equalsInteger i1 3)   -- | B'.) ...在这里应用参数...
            (delay i1)             -- | C'.) ...在这里应用参数,
            (delay 7)
        )
 ) (addInteger 1 2)                -- | D'.) ...然后计算`foo`一次并应用lambda
)
```

Plutarch提供了`plet :: Term s a -> (Term s a -> Term s b) -> Term s b`函数来完成这个。为了演示这种技术，`pf`的实现将导致上述UPLC，如下所示:

```hs
{-
注意: 我们注释上的字母标签与前面示例中的操作相匹配。
-}

pf' :: Term s PInteger
pf' =
  plet (1 + 2) $             -- | D.') 在这里(严格)计算所需的值...
  \foo ->                    -- | A.') ...引入lambda抽象...
    pif
      (foo #== 3)            -- | B.') ...在这里应用参数...
      foo                    -- | C.') ... and here.
      7
```

另一个例子是:

```haskell
abs :: Term s PInteger -> Term s PInteger
abs x = pif (x #<= -1) (negate x) x
```

猜猜如果你像这样使用它会发生什么:

```hs
abs (reallyExpensiveFunction # arg)
```

它会变成:

```hs
pif ((reallyExpensiveFunction # arg) #<= -1) (negate (reallyExpensiveFunction # arg)) (reallyExpensiveFunction # arg)
```

不好了。`reallyExpensiveFunction`将被应用3次。这是成本的3倍！

相反，请考虑使用`plet`:

```haskell
abs :: Term s PInteger -> Term s PInteger
abs x' = plet x' $ \x -> pif (x #<= -1) (negate x) x
```

当然，您真正应该做的是尽可能选择Plutarch级别的函数。因为Plutarch级别函数的参数是预先计算的，而且这些绑定完全可以根据需要多次使用!

## Where should arguments be `plet`ed?

您不必担心每个场景中的参数重复工作。特别是`plam`的参数也是一个Haskell函数，不是吗？但是您不必担心在那里`plet`ing您的参数，因为它通过`plam`变成了Plutarch级别的函数 - 因此，所有参数在传入之前都会被评估。

`plet`还有什么地方不需要？接受延续的函数，例如`plet`(duh)和`pletFields`，总是预先评估绑定。然而，一个例外是`pmatch`。在某些情况下，您不需要在`pmatch` case处理程序中进行`plet`绑定。例如，如果您在`PList`上使用`pmatch`，则`PSCons x xs`中的 `x`和`xs`将始终被预先评估。另一方面，如果您在 `PBuiltinList`上使用`pmatch`，则`PCons x xs`中的`x`和`xs`不会预先评估。如果您多次使用它们，请务必`plem`它们！

一般来说，连续多次`plet`将被优化为一个单一的`plet`。但是，您应该知道对于Data编码类型(遵循[实现`PIsDataRepr`和朋友](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)的类型)和Scott编码类型，`pmatch`处理程序获得预先评估的绑定。对于`PBuiltinList`和`PDataRecord` - 绑定没有预先评估。

你还应该`plet`本地绑定! 特别是，如果您应用一个函数(Plutarch级别或Haskell 级别)来获取一个值，则将该值绑定到一个变量，例如使用`let`或`where`，然后避免多次使用它。绑定将简单地作为函数应用程序内联 - 并且它将继续重新评估。你应该先`plet`它!

这也适用于使用`OverloadedRecordDot`的字段访问。当你执行`ctx.purpose`时，它实际上被翻译成`hrecField @"purpose" ctx`，这是一个函数调用！如果您多次使用该字段，请先`plet`它。

在scott编码类型中可以观察到另一个稍微模糊的情况。当您使用`pcon`构建scott编码类型时 - 您用作字段的Plutarch terms只是内联在scott编码类型中。因此，`pcon $ PPair <complex expr> <another complex expr>`的结果如下:

```hs
(\f -> f <complex expr> <another complex expr>)
```

这实际上是伪代码。但是，它表明在构建scott编码对时不会评估您的表达式。确实，当您对其进行`pmatch`时，它们将被评估。因此，如果您在这`pair`上多次`pmatch`，这些表达式将计算多次！

如果您必须在此类类型上多次`pmatch`，请在构建容器类型之前`plet`字段！
