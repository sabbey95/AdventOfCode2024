# Advent of Code Template

# Table of Contents
- [Advent of Code Template](#advent-of-code-template)
- [Table of Contents](#table-of-contents)
    - [Overview](#overview)
    - [Getting started](#getting-started)

### Overview
This is inspired by mstksg's fantastic Haskell solutions found [here](https://github.com/mstksg/advent-of-code-2020).

This year I'll attempt to write my thoughts on each day's solution, and why this challenge is so much fun in Haskell. You can use this repo as a starter project for writing your own solutions in Haskell as it abstracts away the slightly tricky IO/reading puzzle input from file etc.

### Getting started
See [here](https://www.haskell.org/platform/) for how to install the Haskell platform.
This repo is built using [stack](https://docs.haskellstack.org/en/stable/README/) which you will also need to install. After that, run `stack build` to build the project.

### Getting your GHC/Stack/Cabal version right.
I use [ghcup](https://www.haskell.org/ghcup/) to manage versions of GHC, Cabal and Stack. This is a great way to ensure you have the right versions of everything.
This repo uses [stack.yaml](stack.yaml) to specify a "resolver". This is the stack way of ensuring that versions of dependencies all work with each other. For example, at time of writing it is set to: 
```yaml
resolver:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/21/25.yaml
```
That means that it is using using [this snapshot](https://www.stackage.org/lts-21.25)
You can see a list of snapshots [here](https://www.stackage.org/). The snapshot specifies the versoin of GHC, and ghcup will tell you which versions of cabal and stack work with that version. If the project is not building, it might be because your GHC version is not the right one for this snapshot.


This project uses a .env file for configuration. See `.env.example` to create your own. You can get your session key by logging into Advent of Code then inspecting your cookies. After that, the project will handle getting your puzzle input and caching it in the /res directory.

To solve a day, just open the corresponding DayX.hs file in the /solutions directory. Each solution must be of the form:
```haskell
data AoCSolution a b c =
  MkAoCSolution
    { _parser :: Parser a
    , _solution  :: a -> b
    }
```
See Day1.hs for an example, which has been implemented for day 1.

To run, you can use the GHCI repl. For example:
```
❯ stack ghci
Using main module: 1. Package `AdventOfCode2024' component AdventOfCode2024:exe:AdventOfCode2024-exe with main-is file: /Users/raphael.colman/Dev/AdventOfCode2024/app/Main.hs
AdventOfCode2024> initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: AdventOfCode2024
...

λ> aoc1
Solution:
1602
Solution:
1633
```

This is good for trialling solutions, because `:r` will reload your changes. You can also use the `printTestSolutions` function to use inputs from /res/test_inputs instead

Alternatively, you can build and run the application completely
```
❯ stack build
❯ stack exec AdventOfCode2024-exe
Which day of Advent do you want to solve? [1-25]
1
Solution:
1602
Solution:
1633
```
