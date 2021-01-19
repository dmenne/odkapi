with_mock({
  test_that("Api Client can be created and login/logout is possible", {
    api_client = ApiClient$new(apiaryPath)
    expect_match(api_client$basePath, "odkcentral") # Contains odkcentral
    authentication_api = AuthenticationApi$new(api_client)
    auth = authentication_api$login()$content
    expect_s3_class(auth, "data.frame")
    expect_equal(names(auth), c("createdAt", "expiresAt", "token"))
    # Logout
    logout = authentication_api$logout()$content
    expect_s3_class(logout, "data.frame")
    expect_equal(names(logout), c("success", "message"))
    expect_true(logout$success)
  })
})

  test_that("Api Client with bad address does a timeout", {
    # https://github.com/r-lib/testthat/pull/1194
    # skip_if(httptest_type != "force", silent = TRUE) # for future testthat
    # avoid skip message
    if (httptest_type != "force") return(expect_true(TRUE))
    api_client = ApiClient$new("https://justabitofnonsense.de")
    api_client$timeout = 0.5
    authentication_api = AuthenticationApi$new(api_client)
    auth = authentication_api$login()
    expect_equal(auth$response$status_code, 555)
  })
  
