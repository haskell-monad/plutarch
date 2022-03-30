# Let Haskell level functions take responsibility of evaluation
让Haskell级别的函数负责评估

我们已经讨论了在Plutarch级别上运行的Haskell级别函数如何需要[小心](./Don't%20duplicate%20work.md)处理[工作重复](../Usage/Avoid%20work%20duplication%20using%20plet.md)。与这一点相关的是，设计Haskell级别的函数是一种很好的做法，这样它就可以承担评估的责任。

您的Haskell级别函数的用户不知道它使用了多少次已传递的参数！如果它在没有`plet`的情况下多次使用参数 - 就会有重复的工作！这里有两种解决方案:

- 在将参数传递给Haskell级别函数之前，用户`plet`该参数。
- Haskell级别的函数对其参数负责，并自行`plet`它。

前者是有问题的，因为它是基于假设的。如果Haskell级别的函数是一个很好的规则追随者，并且如果多次使用它，并且正确地`plet`s它的参数怎么办？好吧，那么有一个多余的`plet`(尽管背靠背的`plet`将被优化为一个)。

相反，尝试将评估的责任转移到Haskell级别的函数上 —— 这样它只在需要的时候才`plet`。