# Kevlar CI

Kevlar is an experimental CI/CD system.

## Example

The `example` subdirectory contains a small project.  Its build rules can be
found in the [configuration file](example/.kevlar/config.yml).  You can try
running:

    cd example
    kevlar test

Kevlar is self-hosting, that is it can build itself.  Take a look at [its own
configuration file](.kevlar/config.yml)

## Design

There's a [design document](design.md) explaining the basic concepts of Kevlar.
