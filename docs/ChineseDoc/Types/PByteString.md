# `PByteString`

可以使用`phexByteStr`和`pbyteStr`创建Plutarch级别的bytestring terms。`phexByteStr`将十六进制字符串字面量解释为`Term s PByteString`，而`pbyteStr`仅将`ByteString`转换为`Term s PByteString`。

```haskell
import qualified Data.ByteString as BS

phexByteStr "41"
-- yields a `Term s PByteString`, which represents [65]

pbyteStr (BS.pack [91])
-- yields a `Term s PByteString`, which represents [91]
```

类似于`PString`，它有一个`PEq`实例。以及它的terms的`Semigroup`和`Monoid` 实例。

它没有`PlutusType`实例。

这是Plutus Core[内置字节串](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinByteString)的同义词。