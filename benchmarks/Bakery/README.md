
## Summary

Classical results for Lamport's bakery protocol [1] framed as robustness hyperproperties [2].

## Parameters

* $n$: number of processes in the protocol.

## Models

* `bakery_n`: the bakery protocol with $n$ processes.

## Properties

* `robust_ncrit_n`: for all executions where a process halts in the non-critical section, it does not block liveness other processes (there exists a trace where the halting process behaves the same but others still reach the critical section infinitely often).
* `robust_crit_n`: for any two traces in general, the above property is false, e.g., when a process halts in the critical section and blocks others.

Both are ∀∃ properties over two copies of the `bakery_n` model.

## Examples

| Model 1    | Model 2    | Property         | Result |
| ---------- | ---------- | ---------------- | ------ |
| `bakery_1` | `bakery_1` | `robust_crit_1`  | TRUE   |
| `bakery_2` | `bakery_2` | `robust_crit_2`  | FALSE  |
| `bakery_3` | `bakery_3` | `robust_crit_3`  | FALSE  |
| `bakery_1` | `bakery_1` | `robust_ncrit_1` | TRUE   |
| `bakery_2` | `bakery_2` | `robust_ncrit_2` | TRUE   |
| `bakery_3` | `bakery_3` | `robust_ncrit_3` | TRUE   |

## Non-declarative version

The default version of this example uses declarative `TRANS` clauses in the SMV models. For a non-declarative version, required by AutoHyper, we provide an alternative generation script with explicit `ASSIGN` clauses instead. The same property files can be used for these models.

* `bakery_assigns_n`: the bakery protocol with $n$ processes with `ASSIGN` clauses rather than `TRANS` clauses.

## Parametrized generation

* for `bakery_n`:
  ```
  python bakery.py n
  ```
* for `robust_crit_n.hp`:
  ```
  python robust_crit_n.py n
  ```
* for `robust_ncrit_n.hp`:
  ```
  python robust_ncrit_n.py n
  ```

* for `robust_crit_n` and `robust_ncrit_n`:
  ```
  python robust_crit.py n
  python robust_ncrit.py n
  ```

For the non-declarative versions, use the script version with the `_assigns` suffix.

## References

[1] Leslie Lamport: **A New Solution of Dijkstra's Concurrent Programming Problem.** Commun. ACM 17(8): 453-455 (1974)

[2] Norine Coenen, Bernd Finkbeiner, César Sánchez, Leander Tentrup: **Verifying Hyperliveness.** CAV (1) 2019: 121-139
