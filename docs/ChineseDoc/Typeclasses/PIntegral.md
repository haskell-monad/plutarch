# `PIntegral`

这类似于`Integral`类型类。但是，它只有以下类方法:

- `pdiv` - 类似于`div`
- `pmod` - 类似于`mod`
- `pquot` - 类似于`quot`
- `prem` - 类似于`rem`

使用这些函数，您可以对Plutarch级别的值执行除法/模等操作:

```hs
pdiv # 6 # 3
```

其中`6`和`3`是`Term s PInteger`, 产生`2` —— 它也是一个`Term s PInteger`。