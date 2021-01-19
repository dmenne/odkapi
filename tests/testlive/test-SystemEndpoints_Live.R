if (interactive())
  source(rprojroot::find_package_root_file("tests/testlive/helper-globals_live.R"))

account = local_create_account()
withr::defer(account$authentication$logout()) # cannot be in local_create_account with covr
system = SystemEndpointsApi$new(account$apiClient)

# A path to the browser must be set for interactive tests
browser = Sys.getenv("R_BROWSER")
can_run_interactive = RUN_INTERACTIVE_TESTS && browser != "" & file.exists(browser)
options(browser = browser)


# Audit logs ---------------    
test_that("Can get and filter audit logs", {
  # Make at least one project has been created
  f_api = FormsAndSubmissionsApi$new(account$apiClient)
  project_id = create_test_project(account)$id
  content = system$get_audit_log_entries(action = "project.create", limit = 10)$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content),
                  c('actorId', 'action', 'acteeId', 'name', 'loggedAt', 'details'))
  expect_gt(nrow(content), 1)
})

test_that("Can initiate and complete backup configuration", {
  if (!can_run_interactive)  return(expect_true(TRUE)) # silent skip
  content = system$initiate_new_backup_configuration("askweoi23inc")$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content),  c('token', 'url'))
  expect_equal(nchar(content$token), 64)
  token = content$token
  cat("\nFollow the authorization workflow until the Google token is in the clipboard")
  writeClipboard("")
  browseURL(content$url) # use this in browser
  # Continue until Code is written to clipboard
  while (readClipboard() == "") {
    Sys.sleep(1)
    cat(".")
  }
  google_code = readClipboard()
  cat("\n Google code from clipboard\n", google_code, "\n")
  content = system$complete_new_backup_configuration(token, google_code)$content
  expect_true(content$success)
})

test_that("Can get current backup configuration", {
  if (!can_run_interactive)  return(expect_true(TRUE)) # silent skip
  content = system$get_current_configuration()$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('type', 'setAt', 'recent'))
})

test_that("Can terminate current backup configuration", {
  if (!can_run_interactive)  return(expect_true(TRUE)) # silent skip
  content = system$terminate_current_configuration()$content
  expect_s3_class(content, 'data.frame')
  expect_true(content$success)
  # Try to get current configuration
  content = system$get_current_configuration()$content
  expect_false(content$success)
})

