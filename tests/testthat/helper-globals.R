# Settings from ruODK, see https://docs.ropensci.org/ruODK/articles/setup.html

# ODK server with mocks. Without bearer, this server
# has limits which are handled by delays in ApiClient.R
apiaryPath = "https://private-7855fe-odkcentral.apiary-mock.com" 
email = Sys.getenv("ODKC_UN")
password = Sys.getenv("ODKC_PW")
options(Ncpus = 4L)

library(httptest)
test_directory = rprojroot::find_testthat_root_file()
testdata_directory = rprojroot::find_package_root_file("tests/testdata")
httptest::.mockPaths(test_directory)

# Whenever you make changes to the API:
# Run tests with "force" for normal testing
# When Ok, run tests with "capture" to capture output
# For CRAN and friends: run with "mock" to avoid external dependencies
httptest_type =  "capture" # "capture", "mock", "force"

Sys.setenv(HTTPTEST_TYPE = httptest_type)

with_mock = force
if (httptest_type == "capture") {
  with_mock = httptest::capture_requests 
} else if (httptest_type == "mock") {
  with_mock = httptest::with_mock_api
}


skip_if_no_apiary_server = function(skip_on_mock = FALSE){
  if (skip_on_mock ||  httptest_type != "mock") {
    skip_on_cran()
    skip_on_travis()
    skip_if_not(can_use_server(apiaryPath),  
        paste("Connect to apiary server", apiaryPath))
  }  
}

local_create_account = function(url = apiaryPath, 
                     email = Sys.getenv("ODKC_UN"), 
                     password = Sys.getenv("ODKC_PW"),
                     env = parent.frame()) {
  if (url == apiaryPath) skip_if_no_apiary_server()
  AuthenticatedAccount(url, email, password)
}

if (interactive()) {
  library(testthat)
  library(httr)
}

