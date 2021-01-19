if (interactive())
  source(rprojroot::find_package_root_file('tests/testlive/helper-globals_live.R'))
form_cols = c('version', 'hash', 'sha', 'sha256', 'keyId', 'draftToken', 
              'enketoId', 'publishedAt', 'projectId', 'xmlFormId', 'state', 'name', 'createdAt', 'updatedAt')

account = local_create_account()
withr::defer(account$authentication$logout()) # cannot be used in local_create_account with covr
f_api = FormsAndSubmissionsApi$new(account$apiClient)
project_id = create_test_project(account)$id
filename = file.path(testdata_directory, 'xlsx', 'endo_form.xlsx')

test_that("Can create form", {
  stopifnot(file.exists(filename))
  # Just in case: delete, don't care about result
  content = f_api$delete_form(project_id, 'endo_form' )$content
  expect_equal(names(content), c('success', 'message'))

  # Create 
  content = f_api$create_form(project_id, filename, ignore_warnings = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), form_cols)
})


#test_that('Can retrieve form xml as data frame', {
  content = f_api$retrieve_draft_form_xml_df(project_id, 'endo_form')$content 
  content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), c('success', 'message', 'filename'))
  expect_true(file.exists(content$filename))
  unlink(content$filename)
#})

test_that('Can retrieve form version xml', {
  content =  f_api$retrieve_form_version_xml_file(project_id, 'endo_form', 'endo_form')$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), c('success', 'message', 'filename'))
  expect_true(content$success)
})

