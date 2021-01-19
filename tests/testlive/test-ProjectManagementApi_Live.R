if (interactive())
  source(rprojroot::find_package_root_file("tests/testlive/helper-globals_live.R"))

account = local_create_account()
# cannot be used in local_create_account with covr
withr::defer(account$authentication$logout()) 
withr::defer(delete_user_with_email(account, new_email))

p_api = ProjectManagementApi$new(account$apiClient)
# The following will be assigned with <<-
project_id = NULL
project_name = NULL
actor_id = NULL
role_id  = NULL

test_that("Can create new project", {
  test_project1 = create_test_project(account, p_api)$name
  # Create another random project
  test_project2 = create_test_project(account, p_api)$name
  # Create another one, to be deleted on exit (not required, just to test defer)
  test_project3 = create_test_project(account, p_api)$name

  # List projects
  content = p_api$list_projects()$content
  expect_s3_class(content, "data.frame")
  content = content[is.na(content$archived),]
  expect_setequal(c(test_project1, test_project2, test_project3), content$name)
  # delete second project
  deleted_project = p_api$delete_project(content[2, "id"])$content
  expect_true(deleted_project$success)
  
  # Get project details
  project_id <<- content[1, "id"]
  project_name <<- content[1, "name"]
})

test_that("Can get project_id from name", {
  id = account$project_id_from_name(project_name)
  expect_equal(project_id, id)
})

test_that("Can get project details", {
  detail_project =  p_api$get_project_details(project_id)$content
  expect_s3_class(detail_project, "data.frame")
  expect_equal(detail_project$name, project_name, ignore_attr = TRUE )
  project_names = c("id", "name", "archived", "keyId", "createdAt", "updatedAt")
  expect_equal(names(detail_project), project_names)
})

test_that("Can create new user for testing ", {
  user_cols = c('id', 'type', 'displayName', 'createdAt', 'updatedAt', 'email')
  body = list(email = new_email, password = new_password)
  delete_user_with_email(account, body$email)
  # Create new user
  content = account$create_new_user(body = body)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  user_cols)
  actor_id <<- content$id  
})

test_that("Can list roles", {
  content = account$list_roles()$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('id', 'name', 'system', 'createdAt', 'updatedAt', 'verbs'))
  role_id <<- content[content$system == "formfill", "id"]
})

test_that("Can assign actor to project role", {
  content = p_api$assign_actor_to_project_role(project_id, role_id, actor_id)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})

test_that("Can list actor assigned to project role", {
  content = p_api$list_actors_assigned_project_role(project_id, role_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('id', 'type', 'displayName', 'createdAt', 'updatedAt'))
  expect_equal(content$id, actor_id)
})

test_that("Can list project assignments", {
  content =  p_api$list_project_assignments(project_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('actorId', 'roleId'))
  expect_equal(content$actorId, actor_id)
})

test_that("Can revoke project role assignment", {
  content = p_api$revoke_project_role_assignment_from_actor(
      project_id, role_id, actor_id)$content
  expect_true(content$success)
  # Try again: revoke project role assignment
  content = p_api$revoke_project_role_assignment_from_actor(
    project_id, role_id, actor_id)$content
  expect_false(content$success)
})

test_that("Deep update no implemented ", {
  # Not implemented
  expect_error(p_api$deep_update_project_and_form_details(1, NULL), "Not implemented")
})

test_that("Can enable encryption", {
  body = list(
    passphrase = "super duper secret",
    hint = "it was a secret"
  )
  enc_project = p_api$enable_project_managed_encryption(project_id, body)$content
  expect_s3_class(enc_project, "data.frame")
  expect_true(enc_project$success)
  
  # Try again, should fail
  enc_project = p_api$enable_project_managed_encryption(project_id, body)
  expect_match(enc_project$message, "Could not activate" )
})

test_that("Can update project details. ", {
  # This will archive the project!  
  update_body = list(
    name = project_name,
    archived = TRUE
  )
  update_project = p_api$update_project_details(project_id, body = update_body)$content
  expect_s3_class(update_project, "data.frame")
  expect_equal(update_project$name, update_body$name, ignore_attr = TRUE )
})
