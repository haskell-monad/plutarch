# `PListLike`

`PListLike`类型类为其实例提供了漂亮和熟悉的列表实用程序。Plutarch有两种列表类型 - [`PBuiltinList`](./../Types/PBuiltinList.md)和[`PList`](./../Types/PList.md)。两者都有`PListLike`实例！但是，`PBuiltinList`只能包含内置类型。它不能包含Plutarch函数。`PBuiltinList`的元素类型可以使用`PLift a => PBuiltinList a`来约束。

只要它是`PLift a => PBuiltinList a`或`PList a` - 它就可以访问所有的`PListLike`好东西，开箱即用。它有助于在[`Plutarch.List`](https://github.com/Plutonomicon/plutarch/blob/master/Plutarch/List.hs)中查看其中一些函数。

在此过程中，您可能会遇到两个大问题 ...错误，限制:

```hs
PIsListLike list a
```

这仅仅意味着`list a`类型确实是一个包含有效元素的有效`PListLike`! 当然，所有的`PList a`都是有效的`PListLike`，但我们必须考虑`PBuiltinList`，因为它只能包含`PLift a => a`元素！所以，本质上，一个函数声明为:

```hs
pfoo :: PIsListLike list a => Term s (list a :--> list a)
```

当专门用于`PBuiltinList`时，可以简化为:

```hs
pfoo :: PLift a => Term s (PBuiltinList a :--> PBuiltinList a)
```

就是这样。不要害怕它！

这个怎么样:

```hs
PElemConstraint list a
```

这确保了元素类型`a`确实可以包含在列表类型 - `list`中。对于`PList`，这个约束没有任何意义 —— 它总是正确的。对于`PBuiltinList`，可以简化为`PLift a`。简单！

这是我最喜欢的两个`PListLike`实用程序(没有偏见):

```hs
-- | 将元素添加到现有列表中
pcons :: PElemConstraint list a => Term s (a :--> list a :--> list a)

-- | The empty list
pnil :: PElemConstraint list a => Term s (list a)
```

如果没有`cons`和`nil`，生活会怎样？

让我们用它构建一个`PInteger`的`PBuiltinList`:

```hs
x :: Term s (PBuiltinList PInteger)
x = pcons # 1 #$ pcons # 2 #$ pcons # 3 # pnil
```

哇！不过，我们不要把`PList`单独留在角落里:

```hs
x :: Term s (PList PInteger)
x = pcons # 1 #$ pcons # 2 #$ pcons # 3 # pnil
```

代码是一样的，我们只是改变了类型注解。凉爽的!