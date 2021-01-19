with_mock({
  account = AuthenticatedAccount(apiaryPath)


  test_that("Can list users", {
    content = account$list_users()$content
     expect_s3_class(content, "data.frame")
  })
  
  test_that("Can create Accounts and Users API", {
    expect_match(account$apiClient$basePath, apiaryPath)
  })
  
  test_that("Can List users", {
    content = account$list_users()$content
    expect_s3_class(content, "data.frame")
    expect_equal(as.character(content["email"]),  
                 "my.email.address@opendatakit.org", ignore_attr = TRUE )
  })
  
  test_that("Can list app users", {
    appusers = account$list_app_users(7)$content
    expect_s3_class(appusers, "data.frame")
    expect_equal(ncol(appusers), 7)
  })
  
  test_that("Can create new app user", {
    body = list(
      email = "my.email.address@opendatakit.org",
      password = "abcdefg"
    )
    content = account$create_new_user(body = body)$content
    expect_equal(content$email, "my.email.address@opendatakit.org", ignore_attr = TRUE )
    expect_equal(content$id, 115, ignore_attr = TRUE )
  
  })
  
  test_that("Can get user details", {
    content = account$get_user_details(actor_id = 42)$content            
    expect_equal(content$email, "my.email.address@opendatakit.org", ignore_attr = TRUE  )
    expect_equal(content$id, 115, ignore_attr = TRUE )
  })
    
  
  test_that("Can get authenticated user details", {
    content = account$get_authenticated_user_details()$content
    expect_equal(content$email, "my.email.address@opendatakit.org", ignore_attr = TRUE  )
    # TODO: Extended
  })
  
  
  test_that("Can modify user", {
    body = list(
      email = "new.email.address@opendatakit.org",
      displayName = "New Name"
    )
    content = account$modify_user(actor_id = 42,  body = body)$content
    expect_equal(content$id, 115, ignore_attr = TRUE )
  })
  
  test_that("Can directly update user password", {
    body = list(
      old = "my.email.address@opendatakit.org",
      new = "new.email.address@opendatakit.org"
    )
    content = account$directly_update_user_password(
      actor_id = 42,  body = body)$content
    expect_true(content$success)
  })
  
  test_that("Can initiate password reset", {
    body = list(
      email = "my.email.address@opendatakit.org"
    )
    content = account$initiate_password_reset(body = body)$content
    expect_true(content$success)
    content = account$initiate_password_reset(body = body, TRUE )$content
    expect_true(content$success)
  })
  
  test_that("Can delete user", {
    content = account$delete_user(115)$content
    expect_true(content$success)
  })
  
  
  test_that("Can list assignments", {
    resp = account$list_assignments()
    expect_null(resp$message)
    expect_s3_class(resp$content, "data.frame")
    expect_equal(names(resp$content), c("actorId", "roleId"))
  })
  
  test_that("Can list, assign and strip actors", {
    resp = account$list_actors_assigned_role("admin")
    expect_null(resp$message)
    expect_s3_class(resp$content, "data.frame")
    expect_equal(names(resp$content), c("createdAt", "displayName", "id", "type", "updatedAt"))
    expect_gte(nrow(resp$content), 1)
    
    resp = account$assign_actor_to_server_wide_role("admin", 14)
    expect_true(resp$content$success)
    
    resp = account$strip_role_assignment_from_actor("admin", 14)
    expect_true(resp$content$success)
    
  })
  
  test_that("Can get role details", {
    resp = account$get_role_details(1)$content
    expect_s3_class(resp, "data.frame")
    expect_equal(names(resp),
      c("id", "name", "system", 
        "verbs", "createdAt", "updatedAt"))
    })
  
  test_that("Can list roles", {
    resp = account$list_roles()$content
    expect_s3_class(resp, "data.frame")
    expect_equal(names(resp), 
        c('id','name','system', 'verbs', 'createdAt', 'updatedAt'))
  })

})
