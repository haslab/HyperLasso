
## Summary

Concurrent non-interference for a multi-threaded program, adapted from [1].

## Parameters

* $n$: number of bits in the secret pin.

## Models

* `cni_any_n`: a multi-threaded program that applies an arbitrary mask to a secret pin with $n$ bits.
* `cni_lte_n`: the same model, but where the mask is always less than half the pin.

## Properties

* `cni_n`, ∀∃ over two copies of `cni_any_n` or `cni_lte_n`: concurrent non-interference (for any trace there exists another trace with a different secret pin that reaches the same public output).

## Examples

| Model 1     | Model 2     | Property | Result |
| ----------- | ----------- | -------- | ------ |
| `cni_any_1` | `cni_any_1` | `cni_1`  | FALSE  |
| `cni_any_2` | `cni_any_2` | `cni_2`  | FALSE  |
| `cni_any_3` | `cni_any_3` | `cni_3`  | FALSE  |
| `cni_any_4` | `cni_any_4` | `cni_4`  | FALSE  |
| `cni_lte_1` | `cni_lte_1` | `cni_1`  | FALSE  |
| `cni_lte_2` | `cni_lte_2` | `cni_2`  | FALSE  |
| `cni_lte_3` | `cni_lte_3` | `cni_3`  | TRUE   |
| `cni_lte_4` | `cni_lte_4` | `cni_4`  | TRUE   |

## Non-declarative version

The default version of the `cni_lte_n` mmdel uses declarative `INIT` clauses in the SMV models. For a non-declarative version, required by AutoHyper, we provide alternative generation scripts where the initial condition is moved to the formula as a premise.

* `cni_lte_assigns_n`: the model without the mask restriction defines as a `DEFINE` macro but not imposed as an `INIT` clause.
* `cni_assigns_n`: concurrent non-interference (for any trace there exists another trace with a different secret pin that reaches the same public output) with the mask condition as a premise.

## Parameterized generation

* for `cni_any_n`:
  ```
  python model.py n any
  ```
* for `cni_lte_n`:
  ```
  python model.py n lte
  ```
* for `cni_n`:
  ```
  python spec.py n
  ```

For the non-declarative versions, use the script versions with the `_assigns` suffix.

## References

[1] Geoffrey Smith, Dennis M. Volpano: **Secure Information Flow in a Multi-Threaded Imperative Language.** POPL 1998: 355-364
