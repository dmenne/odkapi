if (interactive())
  source(rprojroot::find_package_root_file("tests/testlive/helper-globals_live.R"))

account = local_create_account()
withr::defer(account$authentication$logout()) # cannot be in local_create_account with covr
project_id = NULL # used with <<-
user_id = NULL

test_that("Can create and list project", {
  # Create test project
  p_api = ProjectManagementApi$new(account$apiClient)
  test_project = create_test_project(account)$name 
  
  # List projects
  projects = p_api$list_projects()$content
  expect_true(test_project %in% projects$name)
  project_id <<- projects[projects$name == test_project, "id"]
  expect_type(project_id, "integer")
})

test_that("Can create user", {    
  body = list(displayName = "testuser")
  content = account$create_new_app_user(project_id, body)$content
  expect_s3_class(content, "data.frame")
  expect_setequal(names(content), 
     c("id","type","displayName","createdAt","updatedAt","projectId","token"))
})

test_that("Can list users", {
  users = account$list_app_users(project_id)$content
  expect_s3_class(users, "data.frame")
  expect_true("testuser" %in% users$displayName)
  user_id <<- users$id[1]
})

test_that("Can create app user qr code",{
  # App user QR code
  qr = account$app_user_qr_code(project_id, user_id, FALSE)
  expect_equal(dim(qr), c(69, 69))
})


test_that("Invalid project id or user id errors",{
  expect_error(account$app_user_qr_code(project_id + 1, user_id))
  expect_error(account$app_user_qr_code(project_id, user_id + 1))
})

test_that("Can delete app user", {
  content = account$delete_app_user(project_id, user_id)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})


