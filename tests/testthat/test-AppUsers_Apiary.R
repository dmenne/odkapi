with_mock({
  
  test_that("Create, delete and list app user",{
    account = local_create_account()
    # Connect
    api_client = ApiClient$new(basePath = apiaryPath)
    auth = AuthenticationApi$new(api_client)
    login = auth$login(email, password)
    stopifnot(is.null(login$response$message))
    account = AccountsAndUsersApi$new(api_client, auth)
  
    # List projects
    p_api = ProjectManagementApi$new(api_client)
    content = p_api$list_projects()$content
    expect_s3_class(content, "data.frame")
    expect_equal(names(content), c("id", "name", "keyId", "archived"))
    project_id = content$id
    
    # Create user    
    body = list(displayName = "testuser")
    content = account$create_new_app_user(1, body)$content
    expect_s3_class(content, "data.frame")
    expect_setequal(names(content), 
       c("id","type","displayName","createdAt","updatedAt","projectId","token"))
    
    # List app users
    users = account$list_app_users(1)$content
    expect_s3_class(users, "data.frame")
    expect_true("My Display Name" %in% users$displayName)
    expect_identical(users$id, 115L )
    user_id = users$id
    
    # App user QR code
    qr = account$app_user_qr_code(project_id, user_id, FALSE)
    expect_equal(dim(qr), c(69, 69))
    
    # Delete app user
    resp = account$delete_app_user(1, users$id)
    expect_null(resp$message)
  })
})