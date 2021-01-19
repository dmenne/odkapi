if (interactive())
  source(rprojroot::find_package_root_file("tests/testlive/helper-globals_live.R"))

test_that('Can create authenticated account',{
  account = AuthenticatedAccount(livePath)  
  expect_equal(account$apiClient$basePath, livePath)
})



test_that('Can create authenticated account with email/password',{
  account = AuthenticatedAccount(livePath, email, password)  
  expect_equal(account$apiClient$basePath, livePath)
})

test_that('Failure when invalid path',{
  expect_error(AuthenticatedAccount("https://nonsense.yy"))
})
