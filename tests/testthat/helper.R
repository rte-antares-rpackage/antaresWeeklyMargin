
# helper to get test files
test_files <- function(subdir, fname = NULL) {
  if (!is.null(fname)) {
    testthat::test_path("files", subdir, fname)
  } else {
    testthat::test_path("files", subdir)
  }
}
