if (interactive())
  source(file.path(
    rprojroot::find_package_root_file('tests/testlive'),
    'helper-globals_live.R'
  ))
form_cols = c('version', 'hash', 'sha', 'sha256', 'keyId','draftToken',
  'enketoId', 'publishedAt','projectId','xmlFormId','state', 'name',
  'createdAt', 'updatedAt','enketoOnceId'
)
submission_cols = c('instanceId', 'submitterId','deviceId', 'createdAt','updatedAt')



# This is an integration test which has to run in sequence. For each call
# to local_create_account, a randomly named (test_1234) test-project is created
# and deleted on exit using deferred execution of testthat
account = local_create_account(livePath)
withr::defer(account$authentication$logout()) # cannot be in local_create_account with covr
f_api = FormsAndSubmissionsApi$new(account$apiClient)
project_id = create_test_project(account)$id

# I wish I would know a better method than using <<- for
# multiply-used variables
instance_id = NULL # Set via <<-
filename = NULL
submission_filename = NULL

test_that('Can Create form', {
  # This will produce a nasty warning which I hope will disappear one day
  filename = file.path(testdata_directory, 'xlsx', 'repeat.xlsx')
  stopifnot(file.exists(filename))
  content = f_api$create_form(project_id, filename, publish = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_match(content$message, 'Repeat behavior has changed')
  
  # Create published form, ignore warnings
  content = f_api$create_form(project_id,
                              filename,
                              ignore_warnings = TRUE,
                              publish = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content), form_cols)
})

test_that('Can list expected form attachments', {
  content = f_api$list_form_attachments(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('name', 'type', 'exists'))
})

test_that('Can get form details', {
  content = f_api$get_form_details(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content),  form_cols)
  expect_equal(content$xmlFormId, "repeat")
  token = content$hash
})

test_that('Can get form schema fields', {
  content = f_api$get_form_schema_fields(project_id, "repeat")$content
  expect_s3_class(content, "data.frame")
  expect_equal(names(content), c("path", "name", "type", "binary"))
})

test_that('Can retrieve form xlsx', {
  content = f_api$retrieve_form_xlsx(project_id, "repeat")$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that('Can retrieve form xlm', {
  content = f_api$retrieve_form_xml_file(project_id, "repeat")$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that('Can create Submission', {
  xml_file = file.path(testdata_directory, 'submissionxml', 'repeat.xml')
  stopifnot(file.exists(xml_file))
  content = f_api$create_submission(project_id, 'repeat', xml_file)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), submission_cols)
})

test_that('Error on invalid file name in submission', {
  expect_error(f_api$create_submission(project_id, 'repeat', "nonsense.txt")$content)
})

test_that('Can export submission to csv', {
  content = f_api$export_submissions_to_csv(project_id, "repeat")$content
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that('Can export submission to csv via post', {
  content = f_api$export_submissions_to_csv_via_post(project_id, "repeat")$content
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that('Can list submissions', {
  content = f_api$list_submissions_on_form(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal( names(content), submission_cols)
  instance_id <<- content$instanceId[1]
})

test_that('Can retrieve submission XML', {
  content = f_api$retrieve_submission_xml(project_id, "repeat", instance_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('success', 'message', 'filename'))
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that('Can retrieve flattened submission', {
  content = f_api$retrieve_submission_flattened(project_id, "repeat", instance_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),
               c('elem', 'elemid', 'value', 'level2', 'level3'))
})

test_that('Can list submission attachments', {
  content = f_api$list_expected_submission_attachments(project_id, "repeat", instance_id)$content
  expect_s3_class(content, "data.frame")
  expect_equal(names(content), c("name", "exists"))
  expect_false(content$exists)
  filename <<- content$name
})

test_that('Can upload submission attachment', {
  upload_file_path = file.path(testdata_directory, "attachments", "greendot.jpg")
  stopifnot(file.exists(upload_file_path))
  content = f_api$upload_submission_attachment(project_id,
                                               "repeat",
                                               instance_id,
                                               filename,
                                               upload_file_path)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})

test_that('Can get submission details', {
  content = f_api$get_submission_details(project_id, "repeat", instance_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content), submission_cols)
  expect_true(is.na(content$deviceId))
  
  #Try again:List submission attachments
  content = f_api$list_expected_submission_attachments(project_id, "repeat", instance_id)$content
  expect_s3_class(content, "data.frame")
  expect_equal(names(content), c("name", "exists"))
  expect_true(content$exists)
  submission_filename <<- content$name
})

test_that('Can download submission attachment', {
  content = f_api$download_submission_attachment(project_id, "repeat", instance_id,
                                                 submission_filename)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})

test_that('Can clear submission attachment', {
  content = f_api$clear_submission_attachment(project_id, "repeat", instance_id, submission_filename)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})

test_that('Can modify form, change name', {
  body = list(name = "newform",  state = "open")
  content = f_api$modify_form(project_id, "repeat", body)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})

test_that('Can list published form versions', {
  content = f_api$list_published_form_versions(project_id, 'repeat')$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content), form_cols)
  expect_equal(content$name, "newform")
  expect_equal(content$xmlFormId, "repeat")
  
})

test_that('Can get form version details', {
  content = f_api$get_form_version_details(project_id, "repeat", "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content), form_cols)
  expect_equal(content$name, "newform")
  expect_equal(content$xmlFormId, "repeat")
  
})

test_that('Can get form version schema fields', {
  content = f_api$get_form_version_schema_fields(project_id, "repeat", "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('path', 'name', 'type', 'binary'))
  expect_setequal(content$binary, c(NA, TRUE))
  expect_setequal(content$type, c("string", "repeat", "binary", "structure"))
})

test_that('Can get form version schema fields with odata', {
  content = f_api$get_form_version_schema_fields(project_id,
                                                 "repeat", "repeat", odata = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('path', 'name', 'type', 'binary'))
  expect_setequal(content$binary, c(NA, TRUE))
  expect_setequal(content$type, c("string", "repeat", "binary", "structure"))
})

test_that('Can list form version attachments', {
  content = f_api$list_form_version_attachments(project_id, "repeat", "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('name', 'type', 'exists'))
  expect_equal(content$name, "name_label.csv")
  filename = content$name
  
})

test_that('Can download form version attachment', {
  content = f_api$download_form_version_attachment(project_id, "repeat", "repeat", filename)$content
  expect_s3_class(content, 'data.frame')
  expect_false(content$success)
  
})

test_that('Can delete form', {
  content = f_api$delete_form(project_id, "repeat")$content
  expect_true(content$success)
  # Try again, should fail
  content = f_api$delete_form(project_id, "repeat")$content
  expect_false(content$success)
})
