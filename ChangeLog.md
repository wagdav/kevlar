# Changelog for kevlar

## Unreleased changes

- Complete rewrite using the Haxl library
- Pipeline definitions are now Haskell code

### Removed
- Dhall-specific code
- Enable the use of `toMap` Dhall builtin
- Enable the use of the `::` operator (new in Dhall v11.0.0)
- Split the "pipeline wiring" from the action specifications

## [0.0.3] - 2019-07-06

- The step definition is changed so that expressing dependencies is now easier
- Re-usable Docker image building component was added in dhall/docker.dhall

## [0.0.2] - 2019-06-11

- A complete rewrite of the initial implementation
- YAML configuration format is replaced by Dhall
- The support for handling secrets using `pass` is temporarilty dropped

## [0.0.1] - 2019-01-10

- Initial implementation.
- Configurable through a YAML file.
- Script: allow environment variables
- Script: allow cache volumes
- Handle artifacts consistently in the pipeline
- Script: use the local user's UID/GID in the container
- Kevlar executes multi-container pipelines, locally
