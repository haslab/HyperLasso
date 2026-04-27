
## Summary

Synthesize a path planning for a robot moving in a board that is guaranteed to avoid enemies, inspired by [1].

## Parameters

* $n$: length of the board side (board size = $n^2$).

## Models

* `robot_n`: movement of a robot on a board with side $n$.

## Properties

* `plan_nxe`, ∃∀ᵉ over $1+e$ copies of the `robot_n` model: the robot can never be caught by any of the $e$ enemies; enemies are normal robots, but each enemy is restricted by the property to only move alongside a specific row.

## Examples

| Model 1   | Models e  | Property   | Result |
| --------- | --------- | ---------- | ------ |
| `robot_3` | `robot_3` | `plan_3x1` | TRUE   |
| `robot_3` | `robot_3` | `plan_3x2` | TRUE   |
| `robot_3` | `robot_3` | `plan_3x3` | FALSE  |
| `robot_4` | `robot_4` | `plan_4x1` | TRUE   |
| `robot_4` | `robot_4` | `plan_4x2` | TRUE   |
| `robot_4` | `robot_4` | `plan_4x3` | TRUE   |
| `robot_4` | `robot_4` | `plan_4x4` | FALSE  |

## Parametrized generation

* for `robot_n`:
  ```
  python robot.py n
  ```
  
* for `plan_nxe`:
  ```
  python plan.py n e
  ```
   
## References

[1] Tzu-Han Hsu, César Sánchez, Borzoo Bonakdarpour: **Bounded Model Checking for Hyperproperties**. TACAS (1) 2021: 94-112

