# ply

A fast, C++-backed chess engine package for R.

Inspired by: https://github.com/kjda/chess-on-go

Funded by: www.chesshere.com

Current status: early but functional (v0.1.0). The core move generator, game registry, SAN replay, and enrichment pipeline are implemented and tested.

## Snapshot

- Version: 0.1.0
- Public API prefixes: `ply_*`, `ply_pgn_*`
- Core engine: C++ bitboard move generator and game registry
- Validation: perft verified through depth 6
- Package check status: `R CMD check --no-tests --no-manual` passed
- Primary use case: chess analysis, feature generation, and R-based ML/AI learning workflows

## Why this package

- Legal move generation from bitboards
- FEN parse and serialize
- SAN to UCI replay helpers
- Managed multi-game registry for interactive play
- Batch feature enrichment for modeling pipelines

## Install

From CRAN (recommended):

```r
install.packages("ply")
```

CRAN package page:

https://cran.r-project.org/package=ply

Development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("plychess/ply-r")
```

## Build and Test

Install from local source (compiles C++ code):

```r
install.packages(".", repos = NULL, type = "source")
```

Run tests from the project root:

```r
install.packages("testthat")
testthat::test_dir("tests/testthat")
```

Or run package checks from the terminal:

```sh
R CMD build .
R CMD check ply_0.1.0.tar.gz
```

Generate the PDF manual:

```sh
R CMD Rd2pdf .
```

Note: PDF generation requires a working LaTeX installation.

## Quick start

```r
library(ply)

state <- ply_game_init()
moves <- ply_legal_moves(state)
state2 <- ply_move_apply(state, "e2e4")
fen <- ply_fen_serialize(state2)
```

## Registry example

```r
id <- ply_game_new("alice")
ply_game_join(id, "bob")
ply_game_move(id, "alice", "e2e4")
info <- ply_game_info(id)
```

## Public API scope

The package exports two public groups:

- Core engine functions with the ply\_\* prefix for state and move logic
- PGN helpers in the ply_pgn\_\* family for parsing tags, extracting movetext, and loading games

## Repository Architecture (ASCII)

```text
        +----------------------+
        |        ply-r         |
        |  R Chess Engine Repo |
        +----------+-----------+
                   |
        +----------v-----------+
        |     R/ public API    |
        | ply_* + ply_pgn_*    |
        +----------+-----------+
                   |
        +----------v-----------+
        |      Rcpp bridge     |
        | RcppExports + bridge |
        +----------+-----------+
                   |
        +----------v-----------+
        |   C++ engine core    |
        | bitboards + registry |
        +----+------------+----+
             |            |
   +---------v--+   +-----v----------------+
   | tests/      |   | docs + package meta |
   | perft 1..6  |   | man, README, NS, CI |
   +-------------+   +----------------------+
```

## Validation snapshot

Recent validation passed with `R CMD check --no-tests --no-manual`, and the package test suite also passed separately during development.

Note: full manual PDF generation may fail locally if LaTeX packages are missing. This is an environment issue, not an engine issue.

## Test Completeness and Perft Validation

The package includes broad automated coverage across core engine logic, edge cases, registry workflows, SAN replay, and batch enrichment. In addition to unit and integration tests, move-generation correctness is validated with perft against published reference node counts, including deep verification through perft depth 6 (119,060,324 nodes) from the starting position. This gives strong confidence in legal move generation and rule handling under both typical and stress conditions.

## Future Work Vision

The roadmap is to keep correctness as the foundation while expanding the package as a practical learning tool for machine learning and introductory AI in R: deep validation is already in place (including perft verification through depth 6), and the next phase is to add learner-friendly examples, modeling-ready feature workflows, and clear statistical analysis paths so R users can move from chess data generation to ML experimentation with minimal setup.
