if (interactive())
  source(file.path( rprojroot::find_package_root_file("tests/testlive"),
                    "helper-globals_live.R"
  ))
form_cols = c('version', 'hash', 'sha', 'sha256', 'keyId', 'draftToken', 
              'enketoId', 'publishedAt', 'projectId', 'xmlFormId', 'state', 
              'name', 'createdAt', 'updatedAt')

account = local_create_account(livePath)
withr::defer(account$authentication$logout()) # cannot be in local_create_account with covr

f_api = FormsAndSubmissionsApi$new(account$apiClient)
project_id = create_test_project(account)$id
instance_id = NULL
submission_filename = NULL
attachment = NULL
draft_token = NULL

test_that("Can create draft form  as an unpublished draft form" ,{
  filename = file.path(testdata_directory, 'xlsx', 'repeat.xlsx')
  stopifnot(file.exists(filename))
  content = f_api$create_form(project_id, filename, publish = FALSE, ignore_warnings = TRUE)$content
  expect_s3_class(content, 'data.frame')
  expect_setequal(names(content), form_cols)
})

test_that("Can list expected draft form attachments" ,{
  content = f_api$list_draft_form_attachments(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('name', 'type', 'exists'))
  attachment <<- file.path(testdata_directory, "attachments", content$name[1])
  expect_true(file.exists(attachment))
})

test_that("Can upload draft form attachment" ,{
  content = f_api$upload_draft_form_attachment(project_id, "repeat", attachment)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
})

test_that("Can download draft form attachment" ,{
  att_file = basename(attachment)
  content = f_api$download_draft_form_attachment(project_id, "repeat",  att_file)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that("Can get draft form details" ,{
  content = f_api$get_draft_form_details(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  form_cols)
  expect_equal(content$xmlFormId, "repeat")
  draft_token <<- content$draftToken
})

test_that("Cancreate QR Code (not an API call)" ,{
  img = f_api$draft_qr_code(project_id, 'repeat', draft_token, 
                      show_image = interactive())
  expect_equal(dim(img), c(69, 69))
})

test_that("Can get draft form schema fields" ,{
  content = f_api$get_draft_form_schema_fields(project_id, "repeat")$content
  expect_s3_class(content, "data.frame")
  expect_equal(names(content), c("path", "name", "type", "binary"))
})

test_that("Can retrieve draft form xlsx as file" ,{
  content = f_api$retrieve_draft_form_xlsx_file(project_id, "repeat")$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that("Can retrieve draft form xlm as file" ,{
  content = f_api$retrieve_draft_form_xml_file(project_id, "repeat")$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that("Can create draft submission" ,{
  xml_file = file.path(testdata_directory, "submissionxml", "repeat.xml" )
  stopifnot(file.exists(xml_file))  
  content = f_api$create_draft_submission(project_id, "repeat", xml_file)$content
  expect_s3_class(content, "data.frame" )
  expect_equal(names(content), c("instanceId", "submitterId", "deviceId", "createdAt", "updatedAt"))
})

test_that("Can export draft submission to csv" ,{
  content = f_api$export_draft_submissions_to_csv(project_id, "repeat")$content
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that("Can export draft submission to csv via post" ,{
  content = f_api$export_draft_submissions_to_csv_via_post(project_id, "repeat")$content
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that("Can list all submissions on draft" ,{
  content = f_api$list_submissions_on_draft_form(project_id, "repeat")$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('instanceId', 'submitterId', 'deviceId', 
                                  'createdAt', 'updatedAt'))
  instance_id <<- content$instanceId
})

test_that("Can get draft submission details" ,{
  content = f_api$get_draft_submission_details(project_id, "repeat", instance_id)$content
  expect_s3_class(content, 'data.frame')
  expect_equal(names(content),  c('instanceId', 'submitterId', 
                                  'deviceId', 'createdAt', 'updatedAt'))
})

test_that("Can retrieve draft submission XML" ,{
  content = f_api$retrieve_draft_submission_xml(project_id, "repeat", instance_id)$content
  expect_true(content$success)
  expect_true(file.exists(content$filename))
  unlink(content$filename)
})

test_that("Can list draft submission encryption keys" ,{
  content = f_api$list_draft_submissions_encryption_keys(project_id, "repeat")$content
  expect_null(content) # TODO: test encryption
})

test_that("Can list draft submission attachments" ,{
  content = f_api$list_expected_draft_submissions_attachments(
    project_id, "repeat", instance_id)$content
  expect_s3_class(content, "data.frame")
  expect_equal(names(content), c("name", "exists"))
  expect_false(content$exists)
  submission_filename <<- content$name  
})

test_that("Can upload draft submission attachment" ,{
  attachment = file.path(testdata_directory, "attachments", "greendot.jpg")
  stopifnot(file.exists(attachment))
  content = f_api$upload_draft_submission_attachment(
      project_id, "repeat", instance_id, submission_filename, attachment)$content
  expect_s3_class(content, "data.frame")  
  expect_true(content$success)
})

test_that("Can repeat:List draft submission attachments" ,{
  content = f_api$list_expected_draft_submissions_attachments(
      project_id, "repeat", instance_id)$content
  expect_s3_class(content, "data.frame")
  expect_equal(names(content), c("name", "exists"))
  expect_true(content$exists)
  submission_filename = content$name  
})

test_that("Can download draft submission attachment" ,{
  content = f_api$download_draft_submission_attachment(
      project_id, "repeat", instance_id, submission_filename)$content
  expect_s3_class(content, "data.frame")  
  expect_true(content$success)
})

test_that("Can clear draft submission attachment" , {
  content = f_api$clear_draft_submission_attachment(
    project_id, "repeat", instance_id, submission_filename)$content
  expect_s3_class(content, "data.frame")
  expect_true(content$success) # exists is FALSE, because no image has been taken
})

test_that("Can delete draft form" ,{
  content = f_api$delete_draft_form(project_id, "repeat")$content
  expect_true(content$success)
})



