
## Summary

Examples of Herman's self-stabilization protocol for a ring of processes [1]. The original protocol considers synchronous process execution. We model the formulation with an asynchronous scheduler presented in [2]. Unlike other self-stabilization protocols, Herman's protocol only guarantees self-stabilization under a fair scheduler.

## Parameters

* $n$: number of processes in the ring.

## Models

* `herman_n`: Herman's protocol for $n$ processes.

## Properties

* `selfstab_[fairness]_n`, ∀∃ over two copies of the `herman_n` model: self-stabilization with different fairness assumptions, namely:
  * `uf`: no fairness assumptions, which is not sufficient for selfstabilization.
  * `gf`: global fairness assumptions in the protocol's coins, which is still not sufficient for selfstabilization.
  * `lf`: local fairness assumptions in the scheduler, which is sufficient for selfstabilization.

## Examples

| Model 1    | Model 2    | Property      | Result |
| ---------- | ---------- | ------------- | ------ |
| `herman_3` | `herman_3` | `selfstab_uf` | FALSE  |
| `herman_5` | `herman_5` | `selfstab_uf` | FALSE  |
| `herman_7` | `herman_7` | `selfstab_uf` | FALSE  |
| `herman_3` | `herman_3` | `selfstab_gf` | FALSE  |
| `herman_5` | `herman_5` | `selfstab_gf` | FALSE  |
| `herman_7` | `herman_7` | `selfstab_gf` | FALSE  |
| `herman_3` | `herman_3` | `selfstab_lf` | TRUE   |
| `herman_5` | `herman_5` | `selfstab_lf` | TRUE   |
| `herman_7` | `herman_7` | `selfstab_lf` | TRUE   |

## Non-declarative version

The default version of this example uses declarative `INIT` clauses in the SMV models. For a non-declarative version, required by AutoHyper, we provide alternative generation scripts where the initial condition is moved to the formula as a premise.

* `herman_assigns_n`: Herman's protocol for $n$ processes (without `INIT` clause).
* `selfstab_uf_assigns_n`: self-stabilization with initial condition and no fairness assumptions, which is not valid for the protocol.
* `selfstab_gf_assigns_n`: self-stabilization with initial condition and global fairness assumptions in the protocol's coins, which is not valid for the protocol.
* `selfstab_lf_assigns_n`: self-stabilization with initial condition and local fairness assumptions in the scheduler, which is valid for the protocol.

## Parametrized generation

* for `herman_n`:
  ```
  python herman.py n
  ```

* for `selfstab_uf_n`, `selfstab_gf_n` and `selfstab_lf_n`:
  ```
  python selfstab_uf.py n
  python selfstab_gf.py n
  python selfstab_lf.py n
  ```

For the non-declarative versions, use the script versions with the `_assigns` suffix.

## Parametrized generation

* for `herman_n.smv`:
  ```
  python robot.py n
  ```
* for `selfstab_uf_n.smv`:
  ```
  python selfstab_uf_n.py n
  ```
* for `selfstab_gf_n.smv`:
  ```
  python selfstab_gf_n.py n
  ```
* for `selfstab_lf_n.smv`:
  ```
  python selfstab_lf_n.py n
  ```

## References

[1] Ted Herman. **Probabilistic Self-Stabilization.** Inf. Process. Lett. 35(2): 63-67 (1990)
[2] Anthony W. Lin, Philipp Rümmer. **Liveness of Randomised Parameterised Systems under Arbitrary Schedulers.** CAV (2) 2016: 112-133
