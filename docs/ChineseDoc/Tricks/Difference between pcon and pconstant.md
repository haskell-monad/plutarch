# The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`

`PlutusType`对于动态构建Plutarch term特别有用 - 即从任意Plutarch term。这是您的Plutarch类型的构造函数包含其他Plutarch term的时候。

`PlutusType`的另一种用途是当你想给你的Plutarch类型一个自定义表示，Scott编码，枚举 - 诸如此类。来自`PlutusType` haddock示例:

```hs
data AB = A | B

instance PlutusType AB where
  type PInner AB _ = PInteger
  pcon' A = 0
  pcon' B = 1
  pmatch' x f = pif (x #== 0) (f A) (f B)
```

您可以在构建过程中使用`A`和`B`构造函数，但仍然可以在后台将您的类型表示为整数！你不能用`pconstant`做到这一点。

当您可以完全从Haskell级别常量构建某些东西并且某些东西具有与Haskell常量相同的表示时，您应该更喜欢`pconstant`/`pconstantData`(来自[`PConstant`/`PLift`](./../Typeclasses/PConstant%20and%20PLift.md))。