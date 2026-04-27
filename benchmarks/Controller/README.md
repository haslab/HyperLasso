
## Summary

Synthesize a controller for two semaphores at an intersection. The controller must satisfy some desired guarantees under some assumptions about the inputs. This is essentially an LTL synthesis problem [1] framed as a hyperproperty.

## Parameters

* $s$: number of states for the synthesized controller
* $t$: number of transitions for the synthesized controller

## Models

* `controller_sxt`: a controller is a Mealy machine that, for input semaphore button presses, decides which semaphores are green; its internal memory can have $s$ states and $t$ transitions. The controller also has a default transition for each state, that occurs when none of the other transitions are enabled.
* `system_s`: the system over which a controller can be run; has an internal state variable ranging over the $s$ possible states.

## Properties

Under strong fairness assumptions that buttons are pressed infinitely often, all system executions must guarantee the liveness requirement that all requests are eventually attended and the safety requirement that two semaphores are not green simultaneously, together with:

* `spec1_sxt`: has the extra guarantee that when a semaphore turns green it stays green for at least two states in a row.
* `spec2_sxt`: has the extra assumption that two semaphore requests cannot appear consecutively.

These are ∃∀ properties over `controller_sxt` and `system_s` models.

## Examples

| Model 1          | Model 2    | Property    | Result |
| ---------------- | ---------- | ----------- | ------ |
| `controller_2x0` | `system_2` | `spec1_2x0` | FALSE  |
| `controller_4x0` | `system_4` | `spec1_4x0` | TRUE   |
| `controller_4x2` | `system_4` | `spec1_4x2` | TRUE   |
| `controller_1x1` | `system_1` | `spec2_1x1` | FALSE  |
| `controller_1x2` | `system_1` | `spec2_1x2` | TRUE   |
| `controller_4x4` | `system_4` | `spec2_4x4` | TRUE   |

## Parametrized generation

* for `controller_sxt.smv`:
  ```
  python controller.py s t
  ```
* for `system_s.smv`:
  ```
  python system.py s
  ```
* for `spec1_sxt.smv` and `spec2_sxt.smv`:
  ```
  python spec1.py s t
  python spec2.py s t
  ```

## References

[1] Peter Faymonville, Bernd Finkbeiner, Leander Tentrup: **BoSy: An Experimentation Framework for Bounded Synthesis.** CAV (2) 2017: 325-332
