# HyperLasso

This repository contains HyperLasso [1], a symbolic bounded model checker for HyperLTL that supports arbitrary liveness and safety properties, for the moment restricted to one quantifier alternation.
HyperLasso receives a set of SMV models, together with a HyperLTL formula, and uses SMT solvers to synthesize counter-example candidates to disprove the formula. These candidates are then checked using a complete non-hyper model checker to confirm that they are indeed true counter-examples. If no counter-example is found, the tool returns that the property maybe be valid (the result is incomplete). If a counter-example is found, the tool returns that the property is definitively invalid (the result is complete).

## Install

Make sure that you have Haskell (GHC + cabal) installed. See [ghcup](https://www.haskell.org/ghcup/install/) for instructions.
Then, simply run:

```
cabal install HyperLasso.cabal --overwrite-policy=always
```

## Run

You can typically run `HyperLasso -i <model1> ... -i <modelN> -F <formula> -b <bmc_bound>`, where:
    
* `<model1> ... <modelN>` are the input models, in SMV format.
* `<formula>` is an hyperproperty over the `N` models, written in HyperLTL.
* `<bmc_bound>` is the maximum unrolling bound `k` used for bounded model checking, starting at `0`

It will return that the property is `TRUE` or `FALSE`.

## Bounded model checking of HyperLTL

By default, for a ∀∃ formula, HyperLasso will try to synthesize a counter-example trace with at most `k` unrolls for the ∀ quantifier, such that no corresponding trace with at most `k`  unrolls exists for the ∃ quantifier. If the property contains liveness requirements, this may yield a false counter-example, because a corresponding trace for the ∃ quantifier may exist with more than `k` unrolls. The dual problem also exists for ∃∀ formulas with safety requirements. HyperLasso starts with bound `0` and increases it until a counter-example is found, or until the user-provided maximum bound is reached.

To check that the synthesized candidate is indeed a valid counter-example, the option `--complete=nuxmv` can be used. In this case, HyperLasso will use the complete symbolic (non-hyper) model checker `NuXmv` to check that the candidate has no corresponding trace for the ∃ quantifier, for any bound on the number of distinct states. 

## Additional usage instructions

You can check all supported options by running:

```
HyperLasso --help
```

## Benchmarks

In the folder `benchmarks` you can find several categories of examples. See more details on the benchmarks and how to run them at [benchmarks](benchmarks).

## Docker

You can find pre-built Docker containers with HyperLasso and all dependencies needed to run its benchmarks at `hugopacheco/hyperlasso` [DockerHub](https://hub.docker.com/repository/docker/hugopacheco/hyperlasso). We have prepared Docker containers for `arm64` and for `amd64`. The correct one is chosen automatically, but you may force a particular `<platform>` with `--platform=<platform>`.

To launch, the Docker container in interactive mode, run:

```
docker run -it hugopacheco/hyperlasso /bin/bash
```

To run the benchmarks inside the container, you would like to mount the benchmarks folder into the container as well. If running docker at the root of this repository, pass the additional option `-v $PWD/benchmarks:/HyperLasso/benchmarks`.

## For developers

If you are looking to read, modify, or extend the source code of HyperLasso (rather than just running it on existing inputs), start with [ARCHITECTURE.md](ARCHITECTURE.md). It describes the pipeline (CLI → parser → boolean IR → SMT/SBV ⇄ nuXmv fallback), maps every module under [src/](src/) to its responsibility, and documents the four most common extension points:

* **Adding a new HyperLTL / SMV operator** — touches [src/SMV/Syntax.hs](src/SMV/Syntax.hs), the Alex/Happy frontend in [src/SMV/](src/SMV/), and the encoding in [src/Transform/SMVToSBV.hs](src/Transform/SMVToSBV.hs).
* **Adding a new input language / frontend** — produce `PackedBmodule` + `Bformula` and the rest of the pipeline ([src/MC.hs](src/MC.hs) onwards) is reused unchanged.
* **Adding a new SMT backend** — already plumbed via `--smtsolver` and the `Solver` enum re-exported from [src/SMT/SBV.hs](src/SMT/SBV.hs); SBV-supported solvers (Z3, CVC5, Boolector, …) work out of the box, others require extending `smtCfg`.
* **Replacing the complete-check backend** (alternative to nuXmv) — implement the two-function interface in [src/SMV/NuXmv.hs](src/SMV/NuXmv.hs) and wire it into `runCompleteMC` in [src/MC.hs](src/MC.hs).

ARCHITECTURE.md also explicitly lists the **hard-coded research-prototype assumptions** (e.g. the `-k 99` BMC bound used by the nuXmv non-emptiness check) so that they are easy to find and change.

For adding new benchmark families, see [benchmarks/README.md](benchmarks/README.md).

## References

[1] Cunha, Alcino, Pacheco, Hugo and Macedo, Nuno. **HyperLasso: Bounded Model Checking of ∀+∃+-Liveness Hyperproperties.** Proceedings of 2026 International Conference on Computer Aided Verification. to appear.

