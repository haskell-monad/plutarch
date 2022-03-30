本节介绍Plutarch中使用的主要类型类.

> 注意: 如果您发现任何错误/有任何本指南无法回答的相关问题，请不要犹豫，立即提出问题。我们的目标是为Plutarch用户提供高质量的文档!

- [`PEq` & `POrd`](./Typeclasses/PEq%20and%20POrd.md)
- [`PIntegral`](./Typeclasses/PIntegral.md)
- [`PIsData`](./Typeclasses/PIsData.md)
- [`PlutusType`, `PCon`, and `PMatch`](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)
  - [Implementing `PlutusType` for your own types (Scott Encoding)](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding)
  - [Implementing `PlutusType` for your own types (`Data` Encoding)](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-data-encoding)
  - [Implementing `PlutusType` for your own types (`newtype`)](./Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-newtype)
- [`PConstant` & `PLift`](./Typeclasses/PConstant%20and%20PLift.md)
  - [Implementing `PConstant` & `PLift`](./Typeclasses/PConstant%20and%20PLift.md#implementing-pconstant--plift)
  - [Implementing `PConstant` & `PLift` for types with type variables (generic types)](./Typeclasses/PConstant%20and%20PLift.md#implementing-pconstant--plift-for-types-with-type-variables-generic-types)
- [`PListLike`](./Typeclasses/PListLike.md)
- [`PIsDataRepr` & `PDataFields`](./Typeclasses/PIsDataRepr%20and%20PDataFields.md)
  - [All about extracting fields](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#all-about-extracting-fields)
    - [Alternatives to `OverloadedRecordDot`](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#alternatives-to-overloadedrecorddot)
  - [All about constructing data values](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#all-about-constructing-data-values)
  - [Implementing `PIsDataRepr` and friends](./Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)
