# Tracing

您可以使用函数`ptrace`、`ptraceError`、`ptraceIfFalse`、`ptraceIfTrue`(来自`Plutarch.Trace`或`Plutarch.Prelude`)进行跟踪。这些行为与您在[PlutusTx](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Trace.html)中使用的行为类似。

如果您打开了`plutarch`的`development`标志 - 您将在脚本评估期间看到跟踪消息出现在跟踪日志中。当不处于开发模式时 - 这些函数基本上什么都不做。