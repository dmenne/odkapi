# Functional tests using a live server given in Sys.getenv("ODKC_URL")
# This can only run on travisCI when an external server is available
# This cannot be run on CRAN

source(file.path( rprojroot::find_package_root_file("tests/testlive"),
           "helper-globals_live.R"))
stopifnot(can_use_server(livePath))
library(testthat)
run_covr = FALSE
devtools::load_all()

if (!run_covr) {
  test_dir(rprojroot::find_package_root_file("tests", "testlive"))
} else {
  test_files = dir(rprojroot::find_package_root_file("tests", "testlive"), 
                   pattern = "test-", full.names = TRUE)
  cat(test_files, sep = "\n")
  source_files = dir(rprojroot::find_package_root_file("R"), pattern = "*.R", full.names = TRUE)
  source_files = c(source_files, file.path( rprojroot::find_package_root_file("tests/testlive"),
                                            "helper-globals_live.R"))
  cv = covr::file_coverage(source_files = source_files, test_files = test_files)
  covr::report(cv)
}

