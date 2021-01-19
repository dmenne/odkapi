with_mock({
  account = AuthenticatedAccount(apiaryPath)
  system = SystemEndpointsApi$new(account$apiClient)
  
  
  test_that("Can get current backup configuration", {
    content = system$get_current_configuration()$content
    expect_s3_class(content, 'data.frame')
    expect_equal(
      names(content),
      c(
        'type',
        'setAt',
        'recent.acteeId',
        'recent.action',
        'recent.actorId',
        'recent.loggedAt',
        'recent.details.success'
      )
    )
  })
  
  test_that("Can get terminate current backup configuration", {
    content = system$terminate_current_configuration()$content
    expect_s3_class(content, 'data.frame')
    expect_true(content$success)
  })
  
  test_that("Can initiate and complete backup configuration", {
    content = system$initiate_new_backup_configuration("pass")$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),  c('token', 'url'))
    expect_equal(nchar(content$token), 64)
    
    google_code = "4/AACdfD3bBxUAI-zlkLodGmUh_IC4zT6-j8EJ7jr7IFF7Nm4RH_S7RHs"
    token = "asdsdlkj"
    content = system$complete_new_backup_configuration(token, google_code)$content
    expect_s3_class(content, 'data.frame')
    expect_true(content$success)
  })
  
  test_that("Can get and filter audit logs", {
    content = system$get_audit_log_entries(
      action = "form.create",
      start = "2000-01-01z",
      end = "2000-12-31T23:59.999z",
      limit = 100,
      offset = 200
    )$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),
                 c('actorId', 'action', 'acteeId', 'loggedAt'))
    # using default (=no) filtering
    content = system$get_audit_log_entries()$content
    expect_s3_class(content, 'data.frame')
    expect_equal(names(content),
                 c('actorId', 'action', 'acteeId', 'loggedAt'))
  })
})
