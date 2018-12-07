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

  script: build.sh         # the transformation and its parameters

  need:
    - src                  # the output of the "src" step is made available

  passed:
    - code_style           # only ordering, no artifacts exchange (?)

  platform: my-image       # the isolation mechanism
```

# Pipeline with build and test stages in two containers

```yaml
steps:

- name: src
  git: {}

- name: builder
  image:
    context: src/docker/build-image
  need:
    - src

- name: tester
  image:
    context: src/docker/tester-image
  need:
    - src

- name: build
  script: gcc src/hello.c -o $out/hello.exe
  need:
    - src
  platform: builder

- name: test
  script:
    - src/run-test.sh build/hello.exe   # src, build are created
    - src/publish_test_results.sh
  need:
    - src
    - build
  platform: tester
```
