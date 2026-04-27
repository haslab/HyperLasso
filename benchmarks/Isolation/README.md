
## Summary

Checking the equivalence of isolation level specifications for transactional databases, with specification following the framework of [1].

## Parameters

* $t$: number of transactions.
* $v$: number of table entries (keys and values).

## Models

* `ser_txv`: database ensuring the Read Committed (RC) isolation level (one transaction is not allowed to read data modified but not yet committed by another transaction) with $t$ transactions and $v$ table entries.
* `rc_txv`: database ensuring the Serializability (SER) isolation level (the results of trying to commit a set of transactions are the same as if the database was non-distributed) with $t$ transactions and $v$ table entries.

## Properties

* `implies_txv`, ∀∃ over two isolation level models: checks if any database ensuring one level also ensures the other, for all workloads with at most $t$ transactions and $v$ table entries; as expected the stronger SER implies RC, but RC does not imply SER due to well-known dirty-read anomalies.

## Examples

| Model 1   | Model 2   | Property      | Result |
| --------- | --------- | ------------- | ------ |
| `rc_2x1`  | `ser_2x1` | `implies_2x1` | TRUE   |
| `rc_3x2`  | `ser_3x2` | `implies_3x2` | FALSE  |
| `rc_4x3`  | `ser_4x3` | `implies_4x3` | FALSE  |
| `ser_2x1` | `rc_2x1`  | `implies_2x1` | TRUE   |
| `ser_3x2` | `rc_3x2`  | `implies_3x2` | TRUE   |
| `ser_4x3` | `rc_4x3`  | `implies_4x3` | TRUE   |

## Parametrized generation

* for `rc_txv`:
  ```
  python rc.py t v
  ```
  
* for `ser_txv`:
  ```
  python ser.py t v
  ```
  
* for `implies_txv`:
  ```
  python implies.py t v
  ```
  
## References

[1] Natacha Crooks, Youer Pu, Lorenzo Alvisi, Allen Clement: **Seeing is Believing: A Client-Centric Specification of Database Isolation.** PODC 2017: 73-82

