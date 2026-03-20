## Test environments

- local: macOS (Apple Silicon), R 4.5.2
- release artifact: ply_0.1.0.tar.gz built by GitHub Actions release workflow
- GitHub Actions CI matrix:
  - ubuntu-latest (R-CMD-check + tests)
  - macos-latest (R-CMD-check + tests)
  - windows-latest (R-CMD-check + tests)

## R CMD check results

- local artifact re-check (macOS, --as-cran --no-manual): 0 errors | 0 warnings | 1 note
  - NOTE: New submission
- GitHub Actions CI (ubuntu + macOS + windows): checks pass

## Notes

This is the first CRAN submission of this package.

The package contains a C++17 engine via Rcpp and includes testthat-based tests. Local checks pass, and GitHub Actions checks also pass across Ubuntu, macOS, and Windows.
