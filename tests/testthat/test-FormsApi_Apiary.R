with_mock({
  account = local_create_account()
  f_api = FormsAndSubmissionsApi$new(account$apiClient)
  form_colnames =  c('projectId', 'xmlFormId', 'name', 'version', 'enketoId', 
                'hash', 'keyId', 'state', 'createdAt', 'updatedAt')
  
  # Forms ----------------------------------------------------------------------
  test_that("Can retrieve form xml data frame", {
    content = f_api$retrieve_form_xml_df(1, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content), c("success", "message", "filename"))
    expect_equal(basename(content$filename), "simple.xml")
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  
  
  test_that("Can retrieve form version xml file", {
    content = f_api$retrieve_form_version_xml_file(1, "simple", "one")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content), c("success", "message", "filename"))
    expect_equal(basename(content$filename), "simple.xml")
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  
  
  test_that("Can list forms", {
    content = f_api$list_all_forms(16)$content
    expect_s3_class(content, "data.frame")
    expect_equal(names(content),  form_colnames)
  })

  test_that("Can create new form from XML", {
    # Testing an invalid form is not possible with apiary since
    # it does not test for validity
    filename = file.path(testdata_directory, "formxml", "valid_form.xml")
    
    # Default options
    content = f_api$create_form(16, filename)$content
    expect_equal(names(content), form_colnames)
    # with options
    content = f_api$create_form(16, filename, 
                  x_xls_form_form_id_fallback = "myid",
                  ignore_warnings = TRUE, publish = TRUE)$content 
    expect_equal(names(content), form_colnames)
    # Use non-existing
    expect_error(f_api$create_form(16, body = "notexist.xml")$content)
  })

  test_that("Can create new form from XLSX", {
    filename = file.path(testdata_directory, "xlsx", "SGTutorial_1.xlsx")
    stopifnot(file.exists(filename))    
    # With default options
    content = f_api$create_form(16, filename)$content
    expect_equal(names(content), form_colnames)
    # with options
    content = f_api$create_form(16, filename, x_xls_form_form_id_fallback = "myid",
                                    ignore_warnings = TRUE, publish = TRUE)$content # no options
    expect_equal(names(content), form_colnames)
  })
  
  test_that("Can get form details", {
    content = f_api$get_form_details(16, "simple")$content
    expect_equal(names(content), form_colnames)
  })
  
  test_that("Can list form attachment", {
    content = f_api$list_form_attachments(16, "simple")$content
    expect_s3_class(content, "data.frame")
    expect_equal(names(content),  c("name", "type", "exists", "updatedAt"))
  })
  
  
  test_that("Can download form attachment", {
    content = f_api$download_form_attachment(16, "simple", "myfile.mp3")$content
    expect_true(content$success)
    expect_true(file.exists(content$filename))
    expect_equal(file.info(content$filename)$size, 13)
    unlink(content$filename)
  })
  
  test_that("Can get form schema fields", {
    content = f_api$get_form_schema_fields(16, "simple")$content 
    expect_s3_class(content, "data.frame")
    expect_equal(names(content), c("name", "path", "type", "binary"))
  }) 

  test_that("Can get form details", {
    content = f_api$get_form_details(16, "simple")$content
    expect_s3_class(content, "data.frame")
    expect_equal(names(content),  form_colnames)
  })
  
  test_that("Can modify form", {
    body = list(name = "A New Name",  state = "open")
    content = f_api$modify_form(16, "simple", body)$content
    expect_s3_class(content, "data.frame")
    expect_true(content$success)
  })

  test_that("Can delete form", {
    content = f_api$delete_form(16, "simple")$content
    expect_s3_class(content, "data.frame")
    expect_true(content$success)
  })

  test_that("Can get form version details", {
    content = f_api$get_form_version_details(1, "simple", "one")$content
    # This does not give a valid response with apiary
    expect_null(content)
  })  
  
  test_that("Can retrieve form version xml file", {
    content = f_api$retrieve_form_version_xml_file(1, "simple", "one")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content), c("success", "message", "filename"))
    expect_equal(basename(content$filename), "simple.xml")
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  
  
  test_that("Can retrieve form version xlsx file", {
    content = f_api$retrieve_form_version_xlsx_file(1, "simple", "one")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content), c("success", "message", "filename"))
    expect_equal(basename(content$filename), "simple.xlsx")
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  
  
  
  test_that("Can retrieve form xml file", {
    # TODO: Apiary does not return the correct xml, 
    # content of file is not XML. 
    # Must use live tests for the real thing.
    content = f_api$retrieve_form_xml_file(1, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content), c("success", "message", "filename"))
    expect_equal(basename(content$filename), "simple.xml")
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  
  
  test_that("Can retrieve form xlsx", {
    content = f_api$retrieve_form_xlsx(1, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content), c("success", "message", "filename"))
    expect_equal(basename(content$filename), "simple.xlsx")
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })  

# Draft  -----------------------------------
  test_that("Can create new draft from XML", {
    filename = file.path(testdata_directory, "formxml", "valid_form.xml")
    skip_if_not(file.exists(filename))
    content = f_api$create_draft_form(1, "simple",  filename)$content # no options
    expect_s3_class(content, "data.frame")#
    expect_true(content$success)
    # with options
    content = f_api$create_draft_form(1, "simple", filename, x_xls_form_form_id_fallback = "myid",
                           ignore_warnings = TRUE)$content # no options
    expect_true(content$success)
    
    # Use non-existing
    expect_error(f_api$create_draft_form(16, "simple", file = "notexist.xml"))
  })

  test_that("Can create new draft from XLSX", {
    filename = file.path(testdata_directory, "xlsx", "SGTutorial_1.xlsx")
    skip_if_not(file.exists(filename))
    content = f_api$create_draft_form(1, "SGTutorial",  filename)$content # no options
    expect_s3_class(content, "data.frame")#
    expect_true(content$success)
    # with options
    content = f_api$create_draft_form(1, "simple", filename, x_xls_form_form_id_fallback = "myid",
                                      ignore_warnings = TRUE)$content # no options
    expect_true(content$success)
  })
  
  test_that("Can retrieve draft form xml as file", {
    content = f_api$retrieve_draft_form_xml_file(1, "simple")$content
    expect_s3_class(content, "data.frame")
    expect_equal(names(content), c("success", "message", "filename"))
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })
  
  test_that("Can retrieve draft form xlsx as file", {
    content = f_api$retrieve_draft_form_xlsx_file(1, "simple")$content 
    expect_equal(names(content), c("success", "message", "filename"))
    # Binary data from apiary server are not valid xlsx
    # TODO: Test with live server
    expect_true(file.exists(content$filename))
    unlink(content$filename)
  })
  
  test_that("Can upload draft form attachment", {
#    if (httptest_type == "force") { # does not work with mock
      content = f_api$upload_draft_form_attachment(1, "simple", 
                   file.path(testdata_directory, "attachments", "bing.mp3"))$content
      expect_true(content$success)
#    }
  })
  
  
  test_that("Can list expected form attachments",{
    content = f_api$list_draft_form_attachments(1, "simple")$content
    expect_s3_class(content, "data.frame")
    expect_equal(names(content),  c("name", "type", "exists", "updatedAt"))
    expect_true(content$exists)
  })  

  test_that("Can clear form attachment", {
    content = f_api$clear_form_attachment(1, "simple", "myfile.mp3")$content
    expect_true(content$success)
  })
  
  test_that("Can get draft form schema fields", {
    content = f_api$get_draft_form_schema_fields(16, "simple")$content 
    expect_s3_class(content, "data.frame")
    expect_equal(names(content), c("name", "path", "type", "binary"))
  }) 
  
  test_that("Can publish draft form", {
    content = f_api$publish_draft_form(1, "simple")$content
    expect_true(content$success)
    content = f_api$publish_draft_form(1, "simple", "newversion")$content
    expect_true(content$success)
  })
  
  test_that("Can delete draft form", {
    content = f_api$delete_draft_form(1, "simple")$content
    expect_true(content$success)
  })
  
  test_that("Can get draft form details", {
    # TODO: This call does not work on apiary and on real server
    content = f_api$get_draft_form_details(1, "simple")$content
    expect_null(content)
  })
  

# Published form Versions ---------------------------------------------
  
  test_that("Can list form version attachments", {
    content = f_api$list_form_version_attachments(1, "simple", "one")$content  
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('name', 'type', 'exists', 'updatedAt'))
  })
  
  test_that("Can list published form versions", {
    response = f_api$list_published_form_versions(1, "simple")$response
    # TODO no valid response on apiary
    expect_equal(response$status_code, 200)
  })
  
  test_that("Can get form version details", {
    content = f_api$list_form_version_attachments(1, "simple", "one")$content  
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('name', 'type', 'exists', 'updatedAt'))
  })
  
  test_that("Can download form version attachment", {
    content = f_api$download_form_version_attachment(1, "simple", "one", "a.csv" )$content
    expect_s3_class(content, "data.frame")
    expect_true(file.exists(content$filename))
    unlink(content$filename)
    expect_true(content$success)
  })
  
  test_that("Can get form version schema fields", {
    content = f_api$get_form_version_schema_fields(16, "simple", "one")$content 
    expect_s3_class(content, "data.frame")
    expect_equal(names(content), c("name", "path", "type", "binary"))
  }) 

  test_that("Can get form version schema fields with odata", {
    content = f_api$get_form_version_schema_fields(16, "simple", "one", odata = TRUE)$content 
    expect_s3_class(content, "data.frame")
    expect_equal(names(content), c("name", "path", "type", "binary"))
  }) 
  
  
# Form assignments --------------------------------------------------------

  test_that("Can list form assignments", {
    content = f_api$list_form_assignments(2, "simple")$content
    expect_s3_class(content, "data.frame")
    expect_equal(names(content),  c("actorId", "roleId"))
  })

  test_that("Can list all actors assigned some form rule",{
    content = f_api$list_actors_assigned_form_role(2, "simple", "manager")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('createdAt', 'displayName', 'id', 'type', 'updatedAt'))
  })
  
  test_that("Can assign actor to form role", {
    content = f_api$assign_actor_to_form_role(2, "simple", "manager", 14)$content
    expect_s3_class(content, "data.frame")
    expect_true(content$success)  
  })
  
  test_that("Can revoke a form role assignment from an actor",{
    content = f_api$revoke_form_role_assignment_from_actor(2, "simple", "manager", 14)$content
    expect_s3_class(content, "data.frame")
    expect_true(content$success)  
  })
  
# Public access links ----------------------------------------------------
  
  test_that("Can list all links",{
    content = f_api$list_links(2, "simple")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('createdAt', 'displayName', 'id', 'type', 'updatedAt', 'token', 'once'))
  })

  test_that("Can create link",{
    body = list(
      displayName = "my public link",
      once =  FALSE
    )
    content = f_api$create_link(2, "simple",  body)$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('createdAt', 'displayName', 'id', 'type', 
                                    'updatedAt', 'token', 'once'))
    expect_equal(content$id, 115)
  })
  
  test_that("Can delete link",{
    content = f_api$delete_link(2, "simple", 42)$content
    expect_s3_class(content, 'data.frame')
    expect_true(content$success)
  })
  
  
})
  