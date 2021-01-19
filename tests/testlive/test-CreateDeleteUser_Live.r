if (interactive())
  source(file.path( rprojroot::find_package_root_file("tests/testlive"),
    "helper-globals_live.R"
  ))

  
account = local_create_account()
  
withr::defer(account$authentication$logout()) # cannot be in local_create_account with covr
  
user_id = NULL
actor_id = NULL

withr::defer(delete_user_with_email(account, new_email))

test_that("Can create user", {
  # Delete user if it exists
  delete_user_with_email(account, new_email)
  
  # Create user
  # Do not make body global, strange error with covr only!
  body = list(email = new_email,  password = new_password)
  resp = account$create_new_user(body = body)
  expect_null(resp$message)
  expect_equal(resp$content$email[[1]], new_email)
  user_id <<- resp$content$id[1]
})

  
test_that("Can make admin", {
  resp = account$assign_actor_to_server_wide_role("admin", user_id)
  expect_true(resp$content$success)
  
  resp = account$list_actors_assigned_role("admin")
  expect_null(resp$message)
  expect_s3_class(resp$content, "data.frame")
  expect_equal(names(resp$content),
               c("id", "type", "displayName", "createdAt", "updatedAt"))
  expect_gte(nrow(resp$content), 1)
  expect_true(new_email %in% resp$content$displayName)
})

test_that("Can revoke admin", {
  resp = account$strip_role_assignment_from_actor("admin", user_id)
  expect_true(resp$content$success)
  
  # Try again to create user, it should fail
  body = list(email = new_email,  password = new_password)
  resp = account$create_new_user(body = body)
  expect_match(resp$message, "already exists")
})

test_that("Can list users with query, new user should be present", {
  users = account$list_users(new_email)$content
  expect_equal(nrow(users), 1)
  expect_equal(users$email[[1]], new_email)
  actor_id <<- users$id[[1]]
})

test_that("Can modify user", {
  new_body = list(email = new_email,
                  displayName = "New Name")
  content = account$modify_user(actor_id = actor_id,  body = new_body)$content
  expect_equal(content$id, actor_id, ignore_attr = TRUE)
  expect_equal(content$displayName, "New Name", ignore_attr = TRUE)
})

test_that("Can initiate password reset", {
  body = list(email = new_email)
  content = account$initiate_password_reset(body = body, invalidate = FALSE)$content
  expect_true(content$success)
})

test_that("Can directly update user password", {
  reallynew_body = list(old = new_password,
                        new = "newerpassword")
  content = account$directly_update_user_password(actor_id = actor_id,  body = reallynew_body)$content
  expect_true(content$success)
  
})

test_that("Can delete user", {
  resp = account$delete_user(actor_id)
  expect_null(resp$message)
  # List users with query, it should be empty
  users = account$list_users(new_email)$content
  expect_null(users, 0)
})

