# What is the `s`?

`s`本质上代表上下文，就像`ST`的`s`.

它用于区分`closed`和`open` terms:

- Closed term: `type ClosedTerm = forall s. Term s a`
- 任意 term: `exists s. Term s a`
- 注意: `(exists s. Term s a) -> b` 同构于
- `forall s. Term s a -> b`
