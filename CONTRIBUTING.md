# Contributors Guide

## Reporting Issues

Please [open an issue](https://issues.serokell.io/newIssue?project=AD)
if you find a bug or have a feature request.
Note: you need to login (e. g. using your GitHub account) first.
Before submitting a bug report or feature request, make sure it hasn't already been submitted.

The more detailed your report is, the faster it can be resolved.
If you report a bug, please provide steps to reproduce this bug and revision of code in which this bug reproduces.

## Branching Model

This project uses a variation of the [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow) branching model with two branches. Naming of long-lived branches is different:
* `develop` branch from OneFlow is called `master` in this repository.
* `master` branch from OneFlow is called `production` in this repository.

## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve our project, pull requests are most welcome.

Our pull request template contains a [checklist](.github/pull_request_template.md#white_check_mark-checklist-for-your-pull-request) of acceptance criteria for your pull request.
Please read it before you start contributing and make sure your contributions adhere to this checklist.

Majority of Ariadne uses
[Universum](https://github.com/serokell/universum) as a custom
prelude. Knit uses default prelude and will likely be moved to a
separate repository at some point.

### Code Style

We use the [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) tool to
prettify Haskell code.

Please note that there is `.stylish-haskell.yaml` in the root of the repository. This
configuration file requires `stylish-haskell` version `0.8.1.0` or newer.

## Makefile

We have a [Makefile](Makefile) which provides shortcuts for the most
common developers' activities, like building with flags suitable for
development, testing, applying `stylish-haskell` and `hlint`, building
Haddock documentation.
