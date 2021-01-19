# Test for forms api with XSLT transformations
# Use odkapi_choices.xsl, okdapi_form.xsl and odkapi_languages.xsl in inst/extdata

with_mock({
  account = local_create_account()
  f_api = FormsAndSubmissionsApi$new(account$apiClient)

  test_that("XSLT files are read when required", {
    expect_equal(class(account$form_style)[1], "xml_document")
    expect_equal(class(account$choices_style)[1], "xml_document")
    expect_equal(class(account$languages_style)[1], "xml_document")
  })

  if (FALSE) {  
  ####### TODO: Apiary does not correctly return form  
  test_that("Can retrieve form xml  xml converted to data frame", {
    content = f_api$retrieve_form_xml_df(1, "simple")$content
    expect_s3_class(content, 'data.frame')
  })  

  test_that("Can retrieve form version xml converted to data frame", {
    content = f_api$retrieve_form_version_xml_df(1, "simple", "one")$content
    expect_s3_class(content, 'data.frame')
  })  
  
  }
})