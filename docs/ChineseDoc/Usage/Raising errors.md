# Raising errors

在PlutusTx中，您会使用[`error`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Prelude.html#v:error)函数表示验证失败. 你可以在Plutarch中使用`perror`做同样的事情。

```hs
fails :: Term s (PData :--> PData :--> PData :--> PUnit)
fails = plam $ \_ _ _ -> perror
```