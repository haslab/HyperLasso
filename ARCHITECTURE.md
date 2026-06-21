# HyperLasso — Architecture and Extension Guide

This document is aimed at **developers** who want to read, modify, or extend the
HyperLasso source code. It complements [README.md](README.md), which targets
end users running the tool on existing inputs.

HyperLasso is a research prototype implemented in Haskell. Its job is to decide
∀+∃+ HyperLTL formulas (with one quantifier alternation) over a set of SMV models
by:

1. Translating the bounded-unrolling problem into SMT (via the
   [SBV](https://hackage.haskell.org/package/sbv) library), and
2. Optionally validating synthesised counter-example candidates with a complete
   (non-hyper) LTL model checker — currently
   [nuXmv](https://nuxmv.fbk.eu/) — to rule out spurious witnesses.

If you have only ever run the tool through its CLI, the rest of this document
should give you the mental model needed to navigate the source tree.

## 1. Pipeline at a glance

```
            +-------------------------------------------------+
            |                       CLI                       |
            |                   src/Main.hs                   |
            +------------------------+------------------------+
                                     |
                                     v
            +------------------------+------------------------+
            |  File I/O — read SMV / formula, write traces    |
            |                    src/IO.hs                    |
            +------------------------+------------------------+
                                     |
                                     v
            +------------------------+------------------------+
            |        SMV + HyperLTL parser (Alex/Happy)       |
            |       src/SMV/Lexer.x   src/SMV/Parser.y        |
            |   AST in src/SMV/Syntax.hs, types in Typing.hs  |
            +------------------------+------------------------+
                                     |
                                     v
            +------------------------+------------------------+
            |   Transform/* — packed IR, boolean encoding,    |
            |    minimisation, normalisation, substitution    |
            |              src/Transform/*.hs                 |
            +------------------------+------------------------+
                                     |
                                     v
            +------------------------+------------------------+
            |   BMC state machine — iterate k, negate CEX,    |
            |          dispatch SMT / complete checks         |
            |                    src/MC.hs                    |
            +-----+-----------------------------------+-------+
                  |                                   |
                  v                                   v
       +----------+-----------+         +-------------+-----------+
       |  SMT backend (SBV)   |         |  Complete LTL backend   |
       |   src/SMT/SBV.hs     |         |    src/SMV/NuXmv.hs     |
       |  Transform/SMVToSBV  |         |  (external `nuXmv`)     |
       +----------------------+         +-------------------------+
```

Concretely, one invocation of HyperLasso flows through these stages in order;
each stage is implemented in the module(s) shown above.

## 2. Module map

The table below is the recommended starting point for orientation: it groups
each module by role and gives a one-line summary of its responsibility. Open the
linked files to see the actual definitions.

### Driver and I/O

| Module | Role |
|---|---|
| [src/Main.hs](src/Main.hs) | CLI parsing (`cmdargs`), argument record `Args`, pipeline orchestration. |
| [src/IO.hs](src/IO.hs) | Read SMV/formula files, write temporary models, render witness traces. |

### Frontend (SMV + HyperLTL)

| Module | Role |
|---|---|
| [src/SMV/Lexer.x](src/SMV/Lexer.x) | Alex lexer for SMV + HyperLTL tokens. |
| [src/SMV/Parser.y](src/SMV/Parser.y) | Happy grammar producing `Pmodule` / `Pformula`. |
| [src/SMV/Syntax.hs](src/SMV/Syntax.hs) | Core AST: `Pmodule`, `Pformula`, `Pexpr`, `Ptype`, quantifiers. |
| [src/SMV/Typing.hs](src/SMV/Typing.hs) | Type inference for SMV expressions. |
| [src/SMV/Pretty.hs](src/SMV/Pretty.hs) | Pretty-printer (Prettyprinter) for SMV/HyperLTL. |
| [src/SMV/Packed.hs](src/SMV/Packed.hs) | "Packed" desugared form of SMV modules used internally. |
| [src/SMV/Trace.hs](src/SMV/Trace.hs) | Trace / counter-example data type and parsing of nuXmv output. |

### Intermediate transforms

| Module | Role |
|---|---|
| [src/Transform/Pexpr.hs](src/Transform/Pexpr.hs) | Helpers on parametric expressions and quantifiers. |
| [src/Transform/Bexpr.hs](src/Transform/Bexpr.hs) | Boolean formula IR `Bformula` / `Bexpr` and conversions to/from `Pexpr`. |
| [src/Transform/Bpacked.hs](src/Transform/Bpacked.hs) | Packed boolean modules; "multi-module" form consumed by the SMT encoder. |
| [src/Transform/Normalize.hs](src/Transform/Normalize.hs) | Equivalence-preserving normalisation. |
| [src/Transform/Minimize.hs](src/Transform/Minimize.hs) | Variable / module minimisation. |
| [src/Transform/Rename.hs](src/Transform/Rename.hs) | Renaming utilities (e.g. per-trace variable dimensions). |
| [src/Transform/Substitute.hs](src/Transform/Substitute.hs) | Substitution on expressions/formulas. |
| [src/Transform/SMV.hs](src/Transform/SMV.hs) | SMV-specific rewrites. |
| [src/Transform/SMVToSBV.hs](src/Transform/SMVToSBV.hs) | Encoding of bounded SMV + HyperLTL into SBV symbolic terms. |

### Backends

| Module | Role |
|---|---|
| [src/SMT/SBV.hs](src/SMT/SBV.hs) | SBV wrapper, `Solver` selection, `smtCfg`, `defaultSMTSolver = Z3`. |
| [src/SMV/NuXmv.hs](src/SMV/NuXmv.hs) | nuXmv subprocess driver for complete LTL checking. |

### Core

| Module | Role |
|---|---|
| [src/MC.hs](src/MC.hs) | BMC state machine: `MCState`, `MCResult`, `completeMC`, CEX negation, bound stepping. |

### Utilities

| Module | Role |
|---|---|
| [src/Utils/Parser.hs](src/Utils/Parser.hs) | Parsec helpers shared by the frontend. |
| [src/Utils/Pretty.hs](src/Utils/Pretty.hs) | Pretty-printing helpers. |
| [src/Utils/Error.hs](src/Utils/Error.hs) | Error reporting. |
| [src/Utils/Location.hs](src/Utils/Location.hs) | Source locations for diagnostics. |
| [src/Utils/Misc.hs](src/Utils/Misc.hs) | Miscellaneous utilities (timing refs, `>< `, etc.). |

## 3. Data flow — life of one query

For an input `HyperLasso -i M1.smv -i M2.smv -F phi.hltl -b K [--complete=nuxmv]`:

1. [Main.hs](src/Main.hs) parses CLI flags into `Args`, then calls into
   [IO.hs](src/IO.hs) to read the SMV files and the HyperLTL formula.
2. The Alex/Happy frontend ([SMV/Lexer.x](src/SMV/Lexer.x),
   [SMV/Parser.y](src/SMV/Parser.y)) produces a `Pmodule` per file and a
   `Pformula` for the property. Types are inferred in
   [SMV/Typing.hs](src/SMV/Typing.hs).
3. Each module is desugared and (optionally) minimised through
   [Transform/Bpacked.hs](src/Transform/Bpacked.hs),
   [Transform/Bexpr.hs](src/Transform/Bexpr.hs), and friends to obtain a
   boolean-level `PackedBmodule` / `Bformula`.
4. [MC.hs](src/MC.hs) builds an initial `MCState` (the multi-modules, the
   formula, and the per-quantifier bounds `mcKs`) and enters the iteration
   loop. Each iteration:
   - encodes the current bounded problem into SBV via
     [Transform/SMVToSBV.hs](src/Transform/SMVToSBV.hs),
   - invokes the selected SMT solver through
     [SMT/SBV.hs](src/SMT/SBV.hs) (`smtCfg`, default `Z3`),
   - if a candidate counter-example trace comes back **and**
     `--complete=nuxmv` was requested, validates it against the complete LTL
     model checker in [SMV/NuXmv.hs](src/SMV/NuXmv.hs) via
     `runCompleteMC` / `doCheckLTLSpecNuXMV`,
   - on UNSAT, calls `negateMCFormula` to add the discovered trace as a
     blocking constraint and raises `k` by `stepk`,
   - terminates when a definitive `MCSat` / `MCUnsat` is reached or `k`
     exceeds the user-provided `-b` bound (returns `MCUnknown`).
5. Output (TRUE/FALSE + optional witness traces) is rendered back through
   [IO.hs](src/IO.hs).

The two key data types to keep in mind:

- `MCState` in [src/MC.hs:139-144](src/MC.hs#L139-L144) — the loop-carried
  state (`mcMultis`, `mcFormula`, `mcKs`, `mcPrevKs`).
- `MCResult` in [src/MC.hs:45-48](src/MC.hs#L45-L48) — the outcome
  (`MCSat Int` / `MCUnsat` / `MCUnknown`).

## 4. Extension points

The architecture is monolithic but the modular pipeline above means most
extensions only touch one or two files. The four most common cases:

### 4.1 Add a new HyperLTL / SMV operator

1. Add a constructor to the relevant AST type in
   [src/SMV/Syntax.hs](src/SMV/Syntax.hs) (`Pexpr` for expressions, `Pformula`
   for formula-level constructs).
2. Add a token in [src/SMV/Lexer.x](src/SMV/Lexer.x) and a grammar rule in
   [src/SMV/Parser.y](src/SMV/Parser.y).
3. Extend the boolean IR in [src/Transform/Bexpr.hs](src/Transform/Bexpr.hs)
   (and its `toBformula` / `fromBformula` conversions) if the operator survives
   into the encoding.
4. Add the symbolic encoding in
   [src/Transform/SMVToSBV.hs](src/Transform/SMVToSBV.hs).
5. Add a pretty-printer case in [src/SMV/Pretty.hs](src/SMV/Pretty.hs).
6. If the operator must round-trip through nuXmv, extend
   `normalizeNuXmvExpr` in [src/MC.hs:193-202](src/MC.hs#L193-L202).

### 4.2 Add a new input language / frontend

The boundary between the frontend and the rest of the pipeline is "produces a
list of `PackedBmodule` plus a `Bformula`". An alternative frontend can be
plugged in by:

1. Implementing parsing into the AST exposed in
   [src/SMV/Syntax.hs](src/SMV/Syntax.hs) (or a parallel AST that you then
   translate); and
2. Reusing
   [src/Transform/Bpacked.hs](src/Transform/Bpacked.hs) to obtain the packed
   form expected by [MC.hs](src/MC.hs).

[Main.hs](src/Main.hs) is the place to add a new CLI flag that selects the
alternate frontend.

### 4.3 Add a new SMT backend

Solver selection is already plumbed via the `--smtsolver` CLI flag (see
[Main.hs](src/Main.hs), field `smtsolver :: SMT.Solver`).

1. SBV re-exports `Data.SBV.Dynamic.Solver` (see
   [src/SMT/SBV.hs](src/SMT/SBV.hs#L31)), so any solver SBV supports (Z3, CVC5,
   Boolector, Yices, MathSAT, …) is already a one-line change at the call
   site.
2. To use a solver **not** supported by SBV, replace the call to
   `runWithSBV` / extend `smtCfg` in [src/SMT/SBV.hs](src/SMT/SBV.hs#L485) with
   a custom dispatch that translates the encoded problem to your solver's
   input format. The signature expected by the rest of the pipeline is the
   `SBVResult` returned to [MC.hs](src/MC.hs).

### 4.4 Replace the complete-check backend (alternative to nuXmv)

[src/SMV/NuXmv.hs](src/SMV/NuXmv.hs) exposes two operations:

- `doCheckLTLSpecNuXMV :: Bool -> FilePath -> IO (Maybe Trace)` — check an LTL
  spec against an SMV file, returning a counter-example trace if invalid.
- `doCheckNonEmptyNuXMV :: Bool -> FilePath -> IO Bool` — check whether the
  model has an infinite execution (uses BMC with bound `-k 99`).

To swap nuXmv for another complete LTL/CTL model checker, implement the same
two functions for the new backend and wire them in via `runCompleteMC` in
[src/MC.hs:83-94](src/MC.hs#L83-L94) — for instance by adding a new constructor
to `data CompleteMC` in [src/MC.hs:42](src/MC.hs#L42) and a corresponding
pattern match.

### 4.5 Add a new benchmark family

See [§3 in benchmarks/README.md](benchmarks/README.md). In short:

1. Create a new folder under [benchmarks/](benchmarks/).
2. Add Python generator scripts that import
   [benchmarks/render_formula.py](benchmarks/render_formula.py) — its
   `Formula` class overloads Python operators (`&`, `|`, `~`, …) so that
   HyperLTL formulas can be built compositionally, and `prettyprint(ah=AH)`
   emits either HyperLasso or AutoHyper syntax.
3. Add the family to the metadata file consumed by
   [benchmarks/benchs.py](benchmarks/benchs.py).

## 5. Known hard-coded prototype assumptions

These are *deliberately listed* so that downstream users and reviewers can find
them without code-archaeology. Changing any of them is a one- or two-line edit
in the indicated file.

- **BMC bound for the nuXmv non-emptiness check is hard-coded to `-k 99`**.
  See `nuXmvCheckNonEmptyScript` and the matching `is true`/`no counterexample
  found with bound 99` parsing in
  [src/SMV/NuXmv.hs:20-26,46-55](src/SMV/NuXmv.hs#L20-L55). Bumping the bound
  requires changing both the script string and the output-matching predicate.
- **Fairness probe is hard-coded to `F FALSE`** in the same script
  ([src/SMV/NuXmv.hs:54](src/SMV/NuXmv.hs#L54)). It is the strongest
  unsatisfiable fairness condition; replacing it adapts the non-emptiness
  question.
- **nuXmv is invoked as an external subprocess** via the `shelly` package
  ([src/SMV/NuXmv.hs:13-15](src/SMV/NuXmv.hs#L13-L15)). The `nuXmv` binary
  must be on `PATH`. There is no in-process integration.
- **Default SMT solver is Z3** (`defaultSMTSolver = Z3` in
  [src/SMT/SBV.hs:498-499](src/SMT/SBV.hs#L498-L499)). Override per-run with
  `--smtsolver=<solver>`.
- **One quantifier alternation only** (∀∃ or ∃∀). This is a research-level
  restriction of the algorithm itself, not a code TODO, and is also
  documented in the paper and [README.md](README.md).
- **No in-process timeout for nuXmv.** Timeout control lives at the
  benchmark-runner level ([benchmarks/benchs.py](benchmarks/benchs.py)
  `--timeout`).

## 6. Building, testing, extending

```sh
# build
cabal install HyperLasso.cabal --overwrite-policy=always

# smoke-test a single benchmark family
cd benchmarks && ./benchs.py --group=CMS
```

There is currently **no unit-test suite**; the benchmark suite under
[benchmarks/](benchmarks/) serves as the integration-test surface. When making
a non-trivial change, the recommended workflow is:

1. Build with `cabal build` (or `cabal install`).
2. Run a small representative subset of the benchmark suite, e.g.
   `./benchs.py --group=CMS`, and verify status indicators stay green.
3. For changes that affect the encoding (operators, transforms, SMT
   backend), run at least one family from each problem class
   (one verification, one synthesis).

## 7. Further reading

- Paper: Cunha, Pacheco, Macedo. *HyperLasso: Bounded Model Checking of
  ∀+∃+-Liveness Hyperproperties.* CAV 2026.
- SBV: <https://hackage.haskell.org/package/sbv>
- nuXmv: <https://nuxmv.fbk.eu/>
