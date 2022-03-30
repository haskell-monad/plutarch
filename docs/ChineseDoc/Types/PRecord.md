# `PRecord`

您可以定义和使用`product ADTs`，包括在`Plutarch`中具有与Haskell记录类似的命名字段的记录。对于Haskell数据类型，例如:

```hs
data Circle = Circle{
  x, y :: Integer,
  radius :: Natural
  }
```

Plutarch中的等价物是:

```hs
data Circle f = Circle{
  x, y :: f PInteger,
  radius :: f PNatural
  }
Plutarch.Rec.TH.deriveAll ''Circle
```

每个字段类型都需要包装到种类`PType -> Type`的类型参数`f`中。这是对一种被称为`Higher-Kinded Data`的常见编码风格的轻微修改。

有了这个定义，`PRecord Circle`将是[PlutusType](../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)的一个实例，所以你可以使用常见的`pcon`和`pcon'`来构造它的值, 使用`pmatch`和`pmatch'`来解构它：

```hs
circle :: Term s (PRecord Circle)
circle = pcon $ PRecord Circle{
  x = 100,
  y = 100,
  radius = 50
  }

distanceFromOrigin :: Term s (PRecord Circle :--> PNatural)
distanceFromOrigin = plam $ flip pmatch $ \(PRecord Circle{x, y})-> sqrt #$ projectAbs #$ x * x + y * y
```

您还可能会发现`Plutarch.Rec`中的`rcon`和`rmatch`更方便，因为它们不需要`PRecord`包装。或者，您可以使用同一模块中的`field`访问器访问各个字段，而不是使用`pmatch`或其替代方法：

```hs
containsOrigin :: Term s (PRecord Circle :--> PBool)
containsOrigin = plam $ \c-> distanceFromOrigin # c #< pto c # field radius
```

## letrec

您可以使用记录来定义相互递归的函数，或更一般地（但不太有用）相互递归的值。

```hs
circleFixedPoint :: Term s (PRecord Circle)
circleFixedPoint = punsafeFrom $ letrec $ \Circle{y, radius}-> Circle{
  x = y,
  y = 2 * radius,
  radius = 50
  }
```

## Record Data

您可以使用以下定义为`PRecord Circle`提供[`PIsData`](../Typeclasses/PIsData.md)实例:

```hs
instance RecordFromData Circle
instance PIsData (PRecord Circle) where
  pfromData = readData $ recordFromFieldReaders Circle{
    x = DataReader pfromData,
    y = DataReader pfromData,
    radius = DataReader pfromData
    }
  pdata = writeData $ recordDataFromFieldWriters Circle{
    x = DataWriter pdata,
    y = DataWriter pdata,
    radius = DataWriter pdata
    }
```

如果您的记录有很多字段，而您只需要从`Data`中获取其中的几个，则仅在单个字段上使用`pfromData`会更有效。您可以使用函数`fieldFromData`关注单个字段:

```hs
radiusFromCircleData :: Term s (PAsData (PRecord Circle) :--> PAsData PNatural)
radiusFromCircleData = fieldFromData radius
```
