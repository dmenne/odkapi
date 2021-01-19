if (interactive())
  source(file.path(
    rprojroot::find_package_root_file("tests/testlive"),
    "helper-globals_live.R"
  ))


test_that("login/logout with password/email without using factory class", {
  skip_if(email == "") # Set in helper
  skip_if(password == "")
  api_client = ApiClient$new(basePath = livePath)
  authentication_api = AuthenticationApi$new(api_client)
  
  # Login with password
  auth = authentication_api$login(email, password)$content
  expect_null(auth$message)
  expect_s3_class(auth, "data.frame")
  expect_equal(names(auth), c("token", "csrf", "expiresAt", "createdAt"))
  
  # Check if apiClient authorizes
  acc = AccountsAndUsersApi$new(api_client)
  users = acc$list_users()
  expect_s3_class(users$content, "data.frame")
  
  # Logout
  logout = authentication_api$logout()$content
  expect_s3_class(logout, "data.frame")
  expect_equal(names(logout), c("success", "message"))
  expect_true(logout$success)
  # Try again
  logout = authentication_api$logout()$content
  expect_false(logout$success) # already logged out
  # Try with faked login error
  authentication_api$apiClient = NULL
  logout = authentication_api$logout()$content
  expect_s3_class(logout, "data.frame")
  expect_false(logout$success)
})

test_that("Can login with password from environment", {
  # Password from Sys.getenv implicitly
  api_client = ApiClient$new(basePath = livePath)
  authentication_api = AuthenticationApi$new(api_client)
  auth = authentication_api$login()$content
  expect_equal(names(auth), c("token", "csrf", "expiresAt", "createdAt"))
  token = auth$token
  
  # Login again - this is not useful in general, but a
  # good test to see if the reset of handles works
  auth1 = authentication_api$login()$content
  expect_equal(names(auth1), c("token", "csrf", "expiresAt", "createdAt"))
  logout = authentication_api$logout()
  expect_true(logout$content$success)
})

test_that("Timeout error is reported", {
  api_client = ApiClient$new(basePath = livePath)
  api_client$timeout = 0.01
  authentication_api = AuthenticationApi$new(api_client)
  status = authentication_api$login()$response$status
  expect_equal(status, 555)
})
