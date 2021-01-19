with_mock({
  account = local_create_account()
  f_api = FormsAndSubmissionsApi$new(account$apiClient)
  # Apiary insists on using prefix uuid:, but it is no longer used
  instance_id = "85cb9aff-005e-4edd-9739-dc9c1a829c44"

# Encryption ---------------------
  # TODO This test is only syntactical
  test_that("Can list encryption keys",{
    resp = f_api$list_encryption_keys(16, "simple")$response
    expect_equal(resp$status_code, 200)
  })
  
  test_that("Can list draft encryption keys",{
    resp = f_api$list_draft_submissions_encryption_keys(16, "simple")$response
    expect_equal(resp$status_code, 200)
  })
  
  # Submissions -----------------------------
  test_that("Can list all submissions on a form", {
    content = f_api$list_submissions_on_form(16, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('instanceId', 'submitterId', 'createdAt', 'updatedAt'))
  })

  test_that("Can export form submissions to CSV",{
    # Default: use tempdir
    content = f_api$export_submissions_to_csv(16, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_true(file.exists(content$filename))
    unlink(content$filename)
    # Use explicit file name (also in tempdir, for CRAN)
    filename = tempfile("odkapi", fileext = ".zip")
    content = f_api$export_submissions_to_csv(16, "simple", filename)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_equal(content$filename, filename)
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  

  test_that("Can export form submissions to CSV via POST",{
    # Default: use tempdir
    content = f_api$export_submissions_to_csv_via_post(16, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_true(file.exists(content$filename))
    unlink(content$filename)
    # Use explicit file name (also in tempdir, for CRAN)
    filename = tempfile("odkapi", fileext = ".zip")
    content = f_api$export_submissions_to_csv(16, "simple", filename)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_equal(content$filename, filename)
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  
  
  test_that("Can get submission details",{
    # Apiary insists on using prefix uuid, but it is no longer used
    instance_id = "uuid:85cb9aff-005e-4edd-9739-dc9c1a829c44"
    content = f_api$get_submission_details(16, "simple", instance_id)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('instanceId', 'submitterId', 'createdAt', 'updatedAt'))
    expect_equal(content$instanceId, instance_id)
  })

  test_that("Can retrieve submission XML",{
    content = f_api$retrieve_submission_xml(3, "RepeatTest", instance_id)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    # The file downloaded here is mock-sense, but the call works with live server
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })

  test_that("Can create submission",{
    filename = file.path(testdata_directory,  "submissionxml/simple_submission.xml")
    content = f_api$create_submission(16, "simple", filename )$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('instanceId', 'submitterId', 'createdAt', 'updatedAt'))
    expect_equal(content$submitterId, 23)
  })
  
# Attachments --------------------------
  test_that("Can list expected submission attachments",{
    content = f_api$list_expected_submission_attachments(16, "simple", instance_id)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('name', 'exists'))
    expect_equal(content$exists, c(TRUE, FALSE, TRUE))
  })

  test_that("Can download a submission attachment",{
    content = f_api$download_submission_attachment(16, "simple", instance_id, "file1.jpg")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_true(file.exists(content$filename))
    unlink(file.exists(content$filename))
  })
  
  test_that("Can upload a submission attachment",{
    filename = file.path(testdata_directory,  "attachments", "patient.csv")
    content = f_api$upload_submission_attachment(1, "simple", instance_id, basename(filename), filename)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message'))
    expect_true(content$success)
  })
  
  test_that("Can clear a submission attachment",{
    content = f_api$clear_submission_attachment(1, "simple", instance_id, "file1.jpg")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message'))
    expect_true(content$success)
  })

# Draft submissions  
  test_that("Can list submissions on a draft form", {
    content = f_api$list_submissions_on_draft_form(16, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('instanceId', 'submitterId', 'createdAt', 'updatedAt'))
  })
  
  test_that("Can export draft form submissions to CSV",{
    # Default: use tempdir
    content = f_api$export_draft_submissions_to_csv(16, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_true(file.exists(content$filename))
    unlink(content$filename)
    # Use explicit file name (also in tempdir, for CRAN)
    filename = tempfile("odkapi", fileext = ".zip")
    content = f_api$export_draft_submissions_to_csv(16, "simple", filename)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_equal(content$filename, filename)
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  
  
  test_that("Can export draft form submissions to CSV via POST",{
    # Default: use tempdir
    content = f_api$export_draft_submissions_to_csv_via_post(16, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_true(file.exists(content$filename))
    unlink(content$filename)
    # Use explicit file name (also in tempdir, for CRAN)
    filename = tempfile("odkapi", fileext = ".zip")
    content = f_api$export_draft_submissions_to_csv(16, "simple", filename)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_equal(content$filename, filename)
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  
  
  
  test_that("Can get draft submission details",{
    content = f_api$get_draft_submission_details(16, "simple", instance_id)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('instanceId', 'submitterId', 'createdAt', 'updatedAt'))
    expect_equal(content$submitterId, 23)
  })
  
  test_that("Can retrieve draft submission XML",{
    content = f_api$retrieve_draft_submission_xml(1, "simple", instance_id)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    # The file downloaded here is mock-sense, but the call works with live server
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })
  
  test_that("Can download draft form attachment",{
    content = f_api$download_draft_form_attachment(1, "simple", "file1.jpg")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    # The file downloaded here is mock-sense, but the call works with live server
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })
  
  test_that("Can create draft submission",{
    filename = file.path(testdata_directory,  "submissionxml/simple_submission.xml")
    stopifnot(file.exists(filename))
    content = f_api$create_draft_submission(16, "simple", filename )$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('instanceId', 'submitterId', 'createdAt', 'updatedAt'))
    expect_equal(content$submitterId, 23)
  })
  
  test_that("Can list expected draft submission attachments",{
    content = f_api$list_expected_draft_submissions_attachments(16, "simple", instance_id)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('name', 'exists'))
    expect_equal(content$exists, c(TRUE, FALSE, TRUE))
  })
  
  test_that("Can download a draft submission attachment",{
    content = f_api$download_draft_submission_attachment(16, "simple", instance_id, "file1.jpg")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message', 'filename'))
    expect_true(file.exists(content$filename))
    unlink(file.exists(content$filename))
  })
  
  test_that("Can upload a draft submission attachment",{
    skip_if(covr::in_covr() && httptest_type == "mock") # fails on covr with mock
    filename = file.path(testdata_directory,  "attachments", "patient.csv")
    content = f_api$upload_draft_submission_attachment(1, "simple", instance_id, "patient.csv", filename)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message'))
    expect_true(content$success)
  })
  
  test_that("Can clear a draft submission attachment",{
    content = f_api$clear_draft_submission_attachment(1, "simple", instance_id, "file1.jpg")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('success', 'message'))
    expect_true(content$success)
  })
  

  
})
