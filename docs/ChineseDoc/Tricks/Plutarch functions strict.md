# Plutarch functions are strict

所有Plutarch函数都是严格的。当您使用`papp`(或`#`/`#$` - `papp`的同义词)将Plutarch函数应用于参数时 - 参数将在传递给函数之前进行评估。如果您不想评估参数，可以使用 `pdelay`。

看: [Delay and Force](./../Introduction/Delay%20and%20Force.md).
