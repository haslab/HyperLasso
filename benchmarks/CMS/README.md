
## Summary

Generalized non-interference for a conference management system (CMS). Example adapted from [1].

## Parameters

* $r$: number of reviewers in the CMS.
* $a$: number of articles in the CMS.

## Models

* `cms_[decisions]_rxa`: CMS with $r$ reviewers and $a$ articles, where the editor's criteria for making the final decision for a paper may be:
  * `any`: based on any decision for any paper.
  * `ndet`: based on any decision for that same paper.
  * `max`: the best decision for that same paper.

## Properties

* `gni_rxa`, ∀∀∃ over three copies of the same CMS model: generalized non-interference (for any two traces of the system, there exists a third trace with the low events of one trace and the high events of the other trace); low and high events are determined based on the views of a designated reviewer.

## Examples

| Model 1        | Model 2        | Model 3        | Property  | Result |
| -------------- | -------------- | -------------- | --------- | ------ |
| `cms_any_1x1`  | `cms_any_1x1`  | `cms_any_1x1`  | `gni_1x1` | TRUE   |
| `cms_any_2x2`  | `cms_any_2x2`  | `cms_any_2x2`  | `gni_2x2` | FALSE  |
| `cms_any_3x3`  | `cms_any_3x3`  | `cms_any_3x3`  | `gni_3x3` | FALSE  |
| `cms_ndet_1x1` | `cms_ndet_1x1` | `cms_ndet_1x1` | `gni_1x1` | TRUE   |
| `cms_ndet_2x2` | `cms_ndet_2x2` | `cms_ndet_2x2` | `gni_2x2` | TRUE   |
| `cms_ndet_3x3` | `cms_ndet_3x3` | `cms_ndet_3x3` | `gni_3x3` | TRUE   |
| `cms_max_1x1`  | `cms_max_1x1`  | `cms_max_1x1`  | `gni_1x1` | TRUE   |
| `cms_max_2x2`  | `cms_max_2x2`  | `cms_max_2x2`  | `gni_2x2` | TRUE   |
| `cms_max_3x3`  | `cms_max_3x3`  | `cms_max_3x3`  | `gni_3x3` | TRUE   |

## Parameterized generation

* for `cms_any_rxa`, `cms_ndet_rxa` and `cms_max_rxa`:
  ```
  python cms_any.py r a
  python cms_ndet.py r a
  python cms_max.py r a
  ```

* for `gni_rxa`:
  ```
  python gni.py r a
  ```

## References

[1] Nuno Macedo, and Hugo Pacheco. **Hyper model checking for high-level relational models.** arXiv preprint arXiv:2512.12024 (2025).
