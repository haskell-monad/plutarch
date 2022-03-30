# Haskell synonym of Plutarch types
# Plutarch类型的Haskell同义词

指南的几个部分使用术语"Haskell同义词"。这是什么意思？它只是应该对应于Plutarch类型的Haskell类型。不一定必须有某种具体的连接(尽管可以有，使用[`PLift`/`PConstant`](./../Typeclasses/PConstant%20and%20PLift.md)) - 它只是一种你可以在思想上建立的联系。

不过，这个细节确实会在具体用例中发挥作用。在将Plutarch代码编译为`Script`后，当您将Haskell数据类型作为参数传递给`Script`时 - 它们显然需要与Plutarch代码的实际参数相对应。

例如，如果Plutarch代码是一个接收`PByteString`的函数，则在编译为`Script`后，您应该传入实际上与`PByteString`共享相同表示的Haskell数据类型 - "Haskell同义词"，可以这么说。在这个例子中，就是`ByteString`\*.

\[\*]: 您实际上不能通过[`Plutus.V1.Ledger.Scripts`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html)API将`ByteString`传递给已编译的`Script`。请注意，您只能使用[`applyArguments`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#v:applyArguments)传递`Data`参数。`Data`的Haskell同义词是`PAsData a`(对于任何`a`)和`PData`.

另看: [弄清楚Plutarch类型的表示](./../Tricks/Representation%20of%20Plutarch%20type.md).
