# Kevlar CI

Kevlar is an experimental CI/CD system.

## Example

The `example` subdirectory contains a small project.  Its build rules can be
found in the [configuration file](example/.kevlar/config.hs).  You can try
running:

    stack exec -- kevlar-example

Kevlar is self-hosting, that is it can build itself.  Take a look at [its own
configuration file](.kevlar/config.hs)


## Release

The release script requires some enviroment variables to be set. The command:

    GITHUB_ACCESS_TOKEN=$(pass api/github.com/kevlar-deployment) VERSION=v0.0.2 kevlar publish

creates a draft release on Github.
