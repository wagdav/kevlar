# Kevlar CI

Kevlar is an experimental CI/CD system.

## Example

The `example` subdirectory contains a small project.  Its build rules can be
found in the [configuration file](example/.kevlar.dhall).  You can try
running:

    cd example
    kevlar test

Kevlar is self-hosting, that is it can build itself.  Take a look at [its own
configuration file](.kevlar.dhall)

## Design

There's a [design document](design.md) explaining the basic concepts of Kevlar.


## Release

The release script requires some enviroment variables to be set. The command:

    GITHUB_ACCESS_TOKEN=$(pass api/github.com/kevlar-deployment) VERSION=v0.0.2 kevlar publish

creates a draft release on Github.
