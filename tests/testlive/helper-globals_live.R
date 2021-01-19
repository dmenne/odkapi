# Settings from ruODK, see https://docs.ropensci.org/ruODK/articles/setup.html
livePath = paste0("https://", Sys.getenv("ODKC_URL")) # Live test server, could be a sandbox
# The live server must have an administrator account with this login
email = Sys.getenv("ODKC_UN")
password = Sys.getenv("ODKC_PW")

# Used to create test accounts
new_email = "new.email@opendatakit.org" 
new_password = "abcd"

# To cleanup unused archived projects, use SQL command:
# truncate table forms CASCADE
# directly in the database. ALL PROJECTS will be deleted!

# Test to create a backup configuration need user intervention and are skipped by default
RUN_INTERACTIVE_TESTS = FALSE
# Required for interactive tests, i.e. Google Backup configuration
# Must be changed on you system
if (RUN_INTERACTIVE_TESTS)
  Sys.setenv(R_BROWSER = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

test_directory = rprojroot::find_package_root_file("tests/testlive")
testdata_directory = rprojroot::find_package_root_file("tests/testdata")

skip_if_no_live_server = function(){
  skip_on_cran()
  skip_on_travis()
  skip_if_not(can_use_server(livePath), 
              paste("Can connect to live server", livePath))
}

local_create_account = function(
                     url = livePath, 
                     email = Sys.getenv("ODKC_UN"), 
                     password = Sys.getenv("ODKC_PW"),
                     env = parent.frame()) {
  skip_if_no_live_server()
  acc = AuthenticatedAccount(url, email, password)
  delete_all_test_projects() # Just in case some are left from previous
  withr::defer(delete_all_test_projects(), envir = env)
  acc
}

delete_all_test_projects = function(account = NULL, p_api = NULL) {
  if (is.null(account)) {
    account = AuthenticatedAccount(livePath)
  } else {
    if (!inherits(account, "AccountsAndUsersApi"))
      stop("create_test_project: p_api is not of class AccountsAndUsersApi") # nocov
  }
  if (is.null(p_api)) {
    p_api = ProjectManagementApi$new(account$apiClient)
  } else {
    if (!inherits(p_api, "ProjectManagementApi")) # nocov
      stop("create_test_project: p_api is not of class ProjectManagementApi") #nocov
  }
  content =  p_api$list_projects()$content
  to_delete = grepl("test_", content$name) & is.na(content$archived)
  id_delete = content$id[to_delete] 
  invisible(lapply(id_delete, p_api$delete_project))
  content$name[to_delete]
}

create_test_project = function(account, p_api = NULL) {
  if (!inherits(account, "AccountsAndUsersApi"))
    stop("create_test_project: p_api is not of class AccountsAndUsersApi") # nocov
  if (is.null(p_api)) {
    p_api = ProjectManagementApi$new(account$apiClient)
  } else {
    if (!inherits(p_api, "ProjectManagementApi"))
      stop("create_test_project: p_api is not of class ProjectManagementApi") # nocov
  }
  body = list(name = glue("test_{floor(runif(1, 10000, 100000))}"))
  p_api$create_project(body)$content[1,c("id", "name")]
}


delete_user_with_email = function(account, email) {
  users = account$list_users(email)$content
  if (!is.null(users)) {
    account$delete_user(users$id[[1]])
    #print("user deleted")
  }
}

# nocov start
if (interactive()) {
  library(testthat)
  library(httr)
  expect_cols = function() {
    ct = deparse(substitute(content, env = parent.frame()),)
    s1 = paste0("expect_s3_class(", ct, ", 'data.frame')\n", sep = "")
    s2 = paste0(paste0("expect_equal(names(", ct, "),  c('",
                       paste0(names(content), collapse = "', '"), "'))"))
    writeClipboard(paste0(s1, s2))
    invisible(NULL) 
  } 
# nocov end
}

