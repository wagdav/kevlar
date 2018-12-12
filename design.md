---
title: Kevlar CI
author: David Wagner
...

# Kevlar - the bulletproof CI/CD system

- reproducible builds
- isolation

# Concepts

Artifact

- Docker image
- directory
- environment variable


Step

- script execution
- docker build
- git checkout


# Building block

```ascii
                       ------
input artifact 1      |      |
----------------------|      | output artifact
                      | Step |----------------
input artifact 2      |      |
----------------------|      |
                       ------
```

A step block can have

- zero or more input artifacts
- exactly one output artifact

There's a one to one correspondence between an artifact and a step that produced
it.

# Step

An example step in YAML:
```yaml
- name: build step
  step:
    type: script           # the transformation and its parameters
    path: build.sh
    platform: my-image     # the isolation mechanism
  need:
    - src                  # the output of the "src" step is made available
  passed:
    - code_style           # only ordering, no artifacts exchange (?)
```
