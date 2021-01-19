if (interactive())
  source(rprojroot::find_package_root_file("tests/testlive/helper-globals_live.R"))


account = local_create_account()
withr::defer(account$authentication$logout()) # cannot be in local_create_account with covr

p_api = ProjectManagementApi$new(account$apiClient)
f_api = FormsAndSubmissionsApi$new(account$apiClient)

project_id = create_test_project(account, p_api)$id
withr::defer(delete_user_with_email(account, new_email))

# To be initialized with <<-
actor_id = NULL
role_id = NULL


test_that('Can create new user',{
  body = list(email = new_email,  password = new_password)
  user_cols = c('id', 'type', 'displayName', 'createdAt', 'updatedAt', 'email')
  # Delete user if it exists
  delete_user_with_email(account, new_email)

  # Create new user
  content = account$create_new_user(body = body)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  user_cols)
  actor_id <<- content$id  
})

test_that('Can create published form',{
  filename = file.path(testdata_directory, 'xlsx', 'repeat.xlsx')
  stopifnot(file.exists(filename))
  content = f_api$create_form(project_id, filename, x_xls_form_form_id_fallback = "repeat",
                              publish = TRUE, ignore_warnings = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('version', 'hash', 'sha', 'sha256', 'keyId', 
                  'draftToken', 'enketoId', 'publishedAt', 'projectId', 'xmlFormId', 
                  'state', 'name', 'enketoOnceId', 'createdAt', 'updatedAt'))
})

test_that('Can get roles',{
  content = account$list_roles()$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('id', 'name', 'system', 'createdAt', 'updatedAt', 'verbs'))
  role_id <<- content[content$system == "manager", "id"]
})

test_that('Can get role details', {
  content = account$get_role_details(role_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('id', 'name', 'system', 'createdAt', 'updatedAt', 'verbs'))
  expect_equal(content$id[1], role_id)  
})

test_that('Can get authenticated user details', {
  content = account$get_authenticated_user_details()$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('id', 'type', 'displayName', 'createdAt', 'updatedAt', 'email'))
  expect_equal(content$displayName, email)
  expect_equal(content$email, email)
})

test_that('Can get user details', {
  content = account$get_user_details(actor_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('id', 'type', 'displayName', 'createdAt', 'updatedAt', 'email'))
  expect_equal(content$displayName, new_email)
  expect_equal(content$email, new_email)
})


test_that('Can assign actor to form role',{
  content = f_api$assign_actor_to_form_role(project_id, "repeat", role_id, actor_id)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})

test_that('Can list all assignments',{
  content =  account$list_assignments()$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content),  c('roleId', 'actorId'))
})

test_that('Can list all form assignments in project',{
  content =  p_api$list_form_assignments_in_project(project_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('xmlFormId', 'roleId', 'actorId'))
  expect_equal(unique(content$xmlFormId), "repeat")
})

test_that('Can list role specific assignments in project',{
  content = p_api$list_role_specific_form_assignments_in_project(project_id, "manager")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('xmlFormId', 'roleId', 'actorId'))
})  