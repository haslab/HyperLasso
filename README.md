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

To launch, for instance, the `arm64` Docker container in interactive mode, run:

```
docker run -it hugopacheco/hyperlasso:arm64 /bin/bash
```

To run the benchmarks inside the container, you would like to mount the benchmarks folder into the container as well. If running docker at the root of this repository, pass the additional option `-v $PWD/benchmarks:/HyperLasso/benchmarks`.

## References

[1] Cunha, Alcino, Pacheco, Hugo and Macedo, Nuno. “HyperLasso: Bounded Model Checking of ∀+∃+-Liveness Hyperproperties.” Proceedings of 2026 International Conference on Computer Aided Verification. to appear.

