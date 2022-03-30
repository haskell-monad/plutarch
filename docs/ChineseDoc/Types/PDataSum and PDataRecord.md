# `PDataSum` & `PDataRecord`


Plutarch的sum和product类型分别使用`PDataSum`和`PDataRecord`表示。这些类型对[`PIsDataRepr`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md)机制至关重要。

每当您需要使用[`Data`编码](./../Concepts/Data%20and%20Scott%20encoding.md#data-encoding)来表示有意义的ADT时，您可能会用到这些。

通常，您将使用 `PDataRecord`。这用于表示构造函数的所有字段:

```hs
import Plutarch.Prelude

newtype Foo (s :: S) = Foo (Term s (PDataRecord '["fooField" ':= PInteger]))
```

`Foo`是一个Plutarch类型，它有一个构造函数和一个字段，名为 `fooField`，类型为`PInteger`。您可以为它[实现`PIsDataRepr`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)，以便将`PAsData Foo`表示为`Constr`编码的数据值.

您可以使用`pdcons`和`pdnil`构建`PDataRecord` terms。这些是熟悉的`cons`和`nil`专用于 `PDataRecord` terms。

```hs
pdcons :: forall label a l s. Term s (PAsData a :--> PDataRecord l :--> PDataRecord ((label ':= a) ': l))

pdnil :: Term s (PDataRecord '[])
```

要将`a`添加到`PDataRecord` term，您必须有`PAsData a`。另一个感兴趣的类型变量是`label`。这只是您要添加的字段的名称。您可以使用类型应用程序来指定字段，或者使用类型注释，或者让GHC匹配类型。

下面是你如何构建一个带有两个整数字段的`PDataRecord`，一个名为`foo`，另一个名为`bar`:

```hs
test ::
test = pdcons @"foo" @PInteger # 7 #$ pdcons @"bar" @PInteger # 42 # pnil
```

另一方面，`PDataSum`更"free-standing"(独立)。特别是以下类型:

```hs
PDataSum
  [ '[ "_0" ':= PInteger
     , "_1" ':= PByteString
     ]
  , '[ "myField" ':= PBool
     ]
  ]
```

表示具有2个构造函数的sum类型。第一个构造函数有两个字段- `_0` 和 `_1`，类型分别为`PInteger`和`PByteString`。第二个构造函数有一个字段 —— `myField`，类型为`PBool`。

> 注意: 按照惯例，给没有规范意义名称的字段命名，例如`_0`、`_1`等。它们只是`第0字段`、`第1个字段`等。