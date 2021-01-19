# This local tests, no mocking required
test_that("Can flatten XML", {
  example <-
    rprojroot::find_package_root_file("tests/testdata/submissionxml/repeat.xml")
  xml <- xml2::read_xml(example, encoding = "bytes")
  df = flattenXML(xml2::xml_root(xml))
  expect_s3_class(df, "data.frame")
  expect_equal(names(df),
               c("elem", "elemid", "value", "level1", "level2", "level3"))
  expect_equal(df[7, "value"], 'hörgerät')
})



xml_files = dir(
  file.path(testdata_directory, "formxml"),
  pattern = "I8.*.xml",
  full.names = TRUE
)

form_style = xml2::read_xml(get_extdata_file("odkapi_form.xsl"))
expect_s3_class(form_style, "xml_document")
languages_style = xml2::read_xml(get_extdata_file("odkapi_languages.xsl"))
expect_s3_class(languages_style, "xml_document")
choices_style = xml2::read_xml(get_extdata_file("odkapi_choices.xsl"))
expect_s3_class(choices_style, "xml_document")
choice_col_names = c("path",
                     "name",
                     "type",
                     "field",
                     "constraint",
                     "calculate",
                     "label",
                     "hint")

test_that("Can return languages", {
  doc = xml2::read_xml(xml_files[1])
  languages_xslt = as.character(xslt::xml_xslt(doc, languages_style, params = list()))
  languages = read.delim(textConnection(as.character(languages_xslt)), header = TRUE)$lang
  expect_equal(languages, c("English (en)", "default", "French (fr)"))
})

test_that("Can process form and languages files", {
  forms = sapply(xml_files, function(xml_file) {
    doc = xml2::read_xml(xml_file)
    languages_xslt = as.character(xslt::xml_xslt(doc, languages_style, params = list()))
    languages = read.delim(textConnection(as.character(languages_xslt)), header = TRUE)$lang
    if (is.null(languages)) languages = "default"
    lapply(languages, function(language) {
      xx = xslt::xml_xslt(doc, form_style, params = list(lang = language))
      d = read.delim(textConnection(as.character(xx)), header = TRUE)
      expect_s3_class(d, "data.frame")
      expect_equal(names(d), choice_col_names)
      d
    })
  })
})

test_that("Can process choices files", {
  choices = sapply(xml_files, function(xml_file) {
    doc = xml2::read_xml(xml_file)
    languages_xslt = as.character(xslt::xml_xslt(doc, languages_style, params = list()))
    languages = read.delim(textConnection(as.character(languages_xslt)), header = TRUE)$lang
    if (is.null(languages)) languages = "default"
    lapply(languages, function(language) {
      xx = xslt::xml_xslt(doc, choices_style, params = list(lang = language))
      d = read.delim(textConnection(as.character(xx)), header = TRUE)
      expect_s3_class(d, "data.frame")
      expect_equal(names(d), c("path", "label", "value"))
      d
    })
  })
  # Check language encoding
  expect_equal(choices[[1]][[3]][3, "label"], "peut-être")
})
