# testthat/setup.R — Store package root for tests to reference.
# testthat resets wd per test file, so we store the root path as an env var.
PROJECT_ROOT <- normalizePath(file.path(getwd(), "..", ".."))
Sys.setenv(TAL_PROJECT_ROOT = PROJECT_ROOT)  # compat alias
Sys.setenv(PLY_ROOT = PROJECT_ROOT)

# Make internal cpp_* functions available in tests without ply::: prefix.
# When running via devtools::test(), load_all() already does this.
# When running via library(ply), we need to attach the namespace.
if (isNamespaceLoaded("ply")) {
  ns <- getNamespace("ply")
  cpp_fns <- ls(ns, pattern = "^cpp_")
  for (fn in cpp_fns) assign(fn, get(fn, envir = ns), envir = globalenv())
}
