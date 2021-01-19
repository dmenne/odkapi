if (interactive())
  source(rprojroot::find_package_root_file('tests/testlive/helper-globals_live.R'))
form_cols = c('version', 'hash', 'sha', 'sha256', 'keyId', 'draftToken', 
  'enketoId', 'publishedAt', 'projectId', 'xmlFormId', 'state', 'name', 'createdAt', 'updatedAt')

account = local_create_account()
withr::defer(account$authentication$logout()) # cannot be used in local_create_account with covr
f_api = FormsAndSubmissionsApi$new(account$apiClient)
project_id = create_test_project(account)$id
filename = file.path(testdata_directory, 'xlsx', 'SGTutorial_1.xlsx')

test_that("Can create draft form", {
  stopifnot(file.exists(filename))
  # Just in case: delete, don't care about result
  content = f_api$delete_form(project_id, 'SGTutorial_1' )$content
  expect_equal(names(content), c('success', 'message'))
  content = f_api$delete_draft_form(project_id, 'SGTutorial_1_new' )$content

  # Create 
  content = f_api$create_form(project_id, filename)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), form_cols)

})

test_that("Create bad file should fail ", {
  expect_error(f_api$create_form(project_id, "somefile.txt")$content)
})

test_that("Can recreate form should fail", {
  content = f_api$create_form(project_id, filename)$content
  expect_false(content$success)
  expect_match(content$message, 'exists')
})

test_that("Can create draft should work  ", {
  content = f_api$create_draft_form(project_id, 'SGTutorial_1', filename)$content
  expect_true(content$success)
  expect_equal(content$message, NA)
})

test_that("Can recreate draft with parameters should also work  ", {
  content = f_api$create_draft_form(project_id, 'SGTutorial_1', filename,
          x_xls_form_form_id_fallback = "SGTutorial_1", ignore_warnings = TRUE)$content
  expect_true(content$success)
  expect_equal(content$message, NA)
})

test_that("Can retrieve draft form details", {
  content = f_api$get_draft_form_details(project_id, 'SGTutorial_1')$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  form_cols)
  expect_equal(content$name, "SGTutorial_1")
})

test_that("Can publish draf", {
  content = f_api$publish_draft_form(project_id, 'SGTutorial_1')$content
  expect_true(content$success)
  expect_equal(content$message, NA)

  # Create new draft
  content = f_api$create_draft_form(project_id, 'SGTutorial_1', filename)$content
  expect_true(content$success)
  expect_equal(content$message, NA)
  
  # Publishing with a new version name works
  content = f_api$publish_draft_form(project_id, 'SGTutorial_1', "SG_Tutorial_1_new")$content
  expect_true(content$success)
  expect_equal(content$message, NA)
})

test_that("Publishing with autoversion works",{
  # Create another draft
  content = f_api$create_draft_form(project_id, 'SGTutorial_1', filename)$content
  expect_true(content$success)
  expect_equal(content$message, NA)
  
  # Publishing with autoversion works (will append name)
  content = f_api$publish_draft_form(project_id, 'SGTutorial_1', auto_version = TRUE)$content
  expect_true(content$success)
  expect_equal(content$message, NA)
})

test_that("Trying to publish form without new version failse",{
  content = f_api$publish_draft_form(project_id, 'SGTutorial_1')$content
  expect_false(content$success)
  expect_match(content$message, 'Could not')
})

test_that("Published form is in list ", {
  content = f_api$list_all_forms(project_id)$content
  expect_s3_class(content, 'data.frame')
  expect_match(content$name, 'SGTutorial_1')  
})

test_that("Can cleanup created forms", {
  content = f_api$delete_form(project_id, 'SGTutorial_1' )$content
  expect_true(content$success)
  # Try again, should fail
  content = f_api$delete_form(project_id, 'SGTutorial_1' )$content
  expect_false(content$success)
})

test_that("Try delete invalid file, should fail", {
  content = f_api$delete_form(project_id, 'anything' )$content
  expect_false(content$success)
})

test_that('Uploading file with error returns useful message',{
  filename = file.path(testdata_directory, 'xlsx', 'bad_SGTutorial_1.xlsx')
  stopifnot(file.exists(filename))
  # Just in case: delete, don't care about result
  content = f_api$delete_form(project_id, 'bad_SGTutorial_1' )$content
  expect_equal(names(content), c('success', 'message'))
  # Create 
  content = f_api$create_form(project_id, filename)$content
  expect_s3_class(content, 'data.frame')
  expect_false(content$success)
  expect_match(content$message, "Error:.*'bad'")
})


test_that('Can upload form with repeats as xlsx', {
  filename = file.path(testdata_directory, 'xlsx', 'repeat.xlsx')
  stopifnot(file.exists(filename))
  # Create. This will produce a nasty warning which I hope will disappear one day
  content = f_api$create_form(project_id, filename, publish = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_match(content$message, "Repeat behavior has changed")
  
  # Try again with ignore_warnings
  content = f_api$create_form(project_id, filename, ignore_warnings = TRUE,
                              publish = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content), c(form_cols, "enketoOnceId"))
})


test_that('Can retrieve form xml', {
  content = f_api$retrieve_form_xml_file(project_id, 'repeat')$content 
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), c('success', 'message', 'filename'))
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that('Can retrieve form xlsx', {
  xfile = tempfile('odkform', fileext = '.xlsx')
  unlink(xfile)
  content = f_api$retrieve_form_xlsx(project_id, 'repeat', xfile)$content 
  expect_equal(names(content), c('success', 'message', 'filename'))
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that('Can list published form versions', {
  content = f_api$list_published_form_versions(project_id, 'repeat')$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('version', 'hash', 'sha', 'sha256', 'keyId', 'draftToken', 'enketoId', 'publishedAt', 'projectId', 'xmlFormId', 'state', 'name', 'enketoOnceId', 'createdAt', 'updatedAt'))
})

test_that('Can get form version details', {
  content = f_api$get_form_version_details(project_id, 'repeat', 'repeat')$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('version', 'hash', 'sha', 'sha256', 'keyId', 'draftToken', 'enketoId', 'publishedAt', 'projectId', 'xmlFormId', 'state', 'name', 'enketoOnceId', 'createdAt', 'updatedAt'))
})

test_that('Can retrieve form version xml', {
  content =  f_api$retrieve_form_version_xml_file(project_id, 'repeat', 'repeat')$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), c('success', 'message', 'filename'))
  expect_true(content$success)
})

test_that('Can retrieve form version xlsx', {
  content =  f_api$retrieve_form_version_xlsx_file(project_id, 'repeat', 'repeat')$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), c('success', 'message', 'filename'))
  expect_true(content$success)
})

