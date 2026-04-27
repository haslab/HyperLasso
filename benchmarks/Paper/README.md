
## Summary

The toy example used in the CAV26 paper.

## Models

* `left`: a simple state machine with 2 states.
* `right`: a simple state machine with 3 states.

## Properties

* `toy`, ∀∃ over the `left` and `right` models: for all left traces, there exists a right trace where atom `a` is eventually true in both models.

## Examples

| Model_1 | Model_2 | Property | Result |
| ------- | ------- | -------- | ------ |
| left    | right   | toy      | FALSE  |

## References

[1] Alcino Cunha, Hugo Pacheco, and Nuno Macedo: **HyperLasso: Bounded Model Checking of ∀+∃+-Liveness Hyperproperties.** CAV 2026. to appear.

