if (interactive())
  source(file.path( rprojroot::find_package_root_file('tests/testlive'),
                    'helper-globals_live.R'
  ))
form_cols = c('version', 'hash', 'sha', 'sha256', 'keyId', 'draftToken', 
              'enketoId', 'publishedAt', 'projectId', 'xmlFormId', 'state', 
              'name', 'createdAt', 'updatedAt')
link_cols = c('id', 'type', 'displayName', 'createdAt', 'updatedAt', 'once', 'token')

# Upload form attachments via draft->published
# actors/forms
# Create/list/delete links

# This is an integration test which has to run in sequence. For each run, 
# a randomly named (test_1234) project is created and deleted on exit
account = local_create_account(livePath)
withr::defer(account$authentication$logout()) # cannot be in local_create_account with covr
f_api = FormsAndSubmissionsApi$new(account$apiClient)
project_id = create_test_project(account)$id
# Used with <<-
role_id = NULL
filename = NULL
attachment = NULL
actor_id = NULL
link_id = NULL

test_that("Can create draft form  as an unpublished form", {
  filename = file.path(testdata_directory, 'xlsx', 'repeat.xlsx')
  stopifnot(file.exists(filename))
  content = f_api$create_form(project_id, filename, publish = FALSE, ignore_warnings = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content), form_cols)
})  

test_that("Can list expected form attachments", {
  content = f_api$list_draft_form_attachments(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('name', 'type', 'exists'))
  filename <<- content$name
})  

test_that("Can upload draft form attachment", {
  attachment <<-  file.path(testdata_directory, "attachments", filename)
  expect_true(file.exists(attachment))
  content = f_api$upload_draft_form_attachment(project_id, "repeat", attachment)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})  

test_that("Can clear form attachment", {
  content = f_api$clear_form_attachment(project_id, "repeat",  basename(attachment))$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})  

test_that("Can re-upload", {
  content = f_api$upload_draft_form_attachment(project_id, "repeat", attachment)$content
  expect_true(content$success)
})  

test_that("Can publish draft form", {
  content = f_api$publish_draft_form(project_id, "repeat", "repeat")$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})  

test_that("Can download form attachment ", {
  content = f_api$download_form_attachment(project_id, "repeat",  basename(attachment))$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})  

# -------- Links  -----------------

test_that("Can create link", {
  body = list(displayName = "my public link", once =  FALSE)
  content = f_api$create_link(project_id, "repeat", body)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  link_cols)

  # Try with parameter once set
  body$once = TRUE
  content = f_api$create_link(project_id, "repeat", body)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  link_cols)
})  

test_that("Can list link", {
  content = f_api$list_links(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), link_cols)
  link_id <<- content$id
})  

test_that("Can delete link", {
  content = f_api$delete_link(project_id, "repeat", link_id[1])$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)

  content = f_api$delete_link(project_id, "repeat", link_id[2])$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
  
  # Try again, should fail
  content = f_api$delete_link(project_id, "repeat", link_id[1])$content
  expect_false(content$success)
})  

# --------------- Actors  and Roles -----------------
test_that("Can delete user if it exists", {
  body = list(email = new_email,
              password = new_password)
  delete_user_with_email(account, new_email)
  # Recreate user
  resp = account$create_new_user(body = body)
  expect_null(resp$message)
  expect_equal(resp$content$email[[1]], new_email)
  actor_id <<- resp$content$id[1]
})  

test_that("Can list roles", {
  content = account$list_roles()$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('id', 'name', 'system', 'createdAt', 'updatedAt', 'verbs'))
  role_id <<- content[content$name == "Project Manager", "id"]
})  

test_that("Can assign actor to role", {
  content = f_api$assign_actor_to_form_role(project_id, "repeat", role_id, actor_id)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
  # Try again, should fail
  content = f_api$assign_actor_to_form_role(project_id, "repeat", role_id, actor_id)$content
  expect_match(content$message, "exists")
})  

test_that("Can list actors assigned form role", {
  content = f_api$list_actors_assigned_form_role(project_id, "repeat", role_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('id', 'type', 'displayName', 'createdAt', 'updatedAt'))
  expect_equal(content$id, actor_id)
  expect_equal(content$type, "user")
})  

test_that("Can list form assignments", {
  content = f_api$list_form_assignments(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('actorId', 'roleId'))
})  

test_that("Can revoke form role assignment from actor", {
  content = f_api$revoke_form_role_assignment_from_actor(project_id, "repeat", role_id, actor_id)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
  # Try again, should fail
  content = f_api$revoke_form_role_assignment_from_actor(project_id, "repeat", role_id, actor_id)$content
  expect_s3_class(content, "data.frame")
  expect_false(content$success)
})  

test_that("Can list encryption keys (none assigned)  ", {
  content = f_api$list_encryption_keys(project_id, "repeat")$content
  expect_null(content)
})
