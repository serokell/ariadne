# Contributors Guide

## Bug Reports

Please [open an issue](https://github.com/serokell/ariadne/issues) if
you find a bug.

The more detailed your report is, the faster it can be resolved.

## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve Ariadne, pull requests are most welcome.

Please make sure your contributions adhere to our coding guidelines:

*  Code must adhere to the [Style Guide](docs/code-style.md).
*  Code should be documented with [Haddock](https://www.haskell.org/haddock/doc/html/index.html).
*  Please refer to [this guide](https://chris.beams.io/posts/git-commit/) to write a good
   Git commit message.

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
development, testing, applying `stylish-haskell`.
