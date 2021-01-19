with_mock({
  account = local_create_account()
  p_api = ProjectManagementApi$new(account$apiClient)
  project_names = c("id","name","keyId","archived")
  # It is unclear why on some tests this is returned, depending on unknown context
  project_names1 = c("id","name","archived","keyId","createdAt","updatedAt")

  test_that("Can list projects",{
    # List projects
    content = p_api$list_projects()$content
    expect_s3_class(content, "data.frame")
    expect_true("Default Project" %in% unique(content$name))
  })
  
  test_that("Can create projects",{
    # Create project
    body = list(name = "test_new")
    new_project = p_api$create_project(body)$content
    expect_s3_class(new_project, "data.frame")
    # Sometimes project_names1 is returned
    expect_true(all(names(new_project) %in% project_names1))    
  })
    
  test_that("Can get project details",{
    new_project_id = 1
    # Get project details
    detail_project =  p_api$get_project_details(new_project_id)$content
    expect_s3_class(detail_project, "data.frame")
    expect_equal(detail_project$name, "Default Project", ignore_attr = TRUE )
    expect_true(all(names(detail_project) %in% project_names1))    
  })
    
  test_that("Update project details",{
    body = list(
      name = "Default Project",
      archived = TRUE
    )
    update_project = p_api$update_project_details(1, body = body)$content
    expect_s3_class(update_project, "data.frame")
    expect_equal(update_project$name, body$name, ignore_attr = TRUE )
    expect_true(all(names(update_project) %in% project_names1))    
  })
  
  test_that("Deep update not yet implemented",{
    body = list(
      name = "Project name",
      archived = FALSE,
      forms = list()
    )
    expect_error(p_api$deep_update_project_and_form_details(2, body), "implemented")
  })
  
  test_that("Can enable encryption", {
    body = list(
      passphrase = "super duper secret",
      hint = "it was a secret"
    )
    enc_project = p_api$enable_project_managed_encryption(1, body)
    expect_null(enc_project$message)
    expect_s3_class(enc_project$content, "data.frame")
    expect_true(enc_project$content$success)
  })
  
  test_that("Can delete project", {
    del = p_api$delete_project(16)$content
    expect_true(del$success)
  })

  test_that("Can list all project assignments", {
    pr = p_api$list_project_assignments(2)$content
    expect_s3_class(pr, "data.frame")
    expect_equal(names(pr), c("actorId", "roleId"))
  })
  
  test_that("Can assign actor to project role ", {
    actor = p_api$assign_actor_to_project_role(2, "manager", 14)$content
    expect_s3_class(actor, "data.frame")
    expect_true(actor$success)  
  })

  test_that("Not implemented deep update project ", {
    expect_error(p_api$deep_update_project_and_form_details(16), 'implemented')
  })
  
  test_that("Can enable project managed encryption", {
    body = list(
      passphrase = "super duper secret",
      hint = "it was a secret"
    )
    enc = p_api$enable_project_managed_encryption(16, body)$content
    expect_s3_class(enc, "data.frame")
    expect_true(enc$success)
  })
  
  test_that("Can list all project assigments", {
    pa = p_api$list_actors_assigned_project_role(2, "manager")$content
    expect_s3_class(pa, "data.frame")
    expect_equal(names(pa), c("createdAt", "displayName", "id", "type", "updatedAt"))
  })
  
  test_that("Can list project assignments", {
    pa =  p_api$list_project_assignments(16)$content
    expect_s3_class(pa, "data.frame")
    expect_equal(names(pa), c("actorId", "roleId"))
  })
  
  test_that("Can revoke project role assignment from actor ", {
    re = p_api$revoke_project_role_assignment_from_actor(2, "manager", 14)$content
    expect_s3_class(re, "data.frame")
    expect_true(re$success)  
  })
  
#  test_that("Can see role-specific form assignments within a project ", {
    # This function is incorrectly implemented in apiary, note the :userid
    # Works with live server
    #re = p_api$list_role_specific_form_assignments_in_project(2, "admin")$content 
    #expect_s3_class(re, "data.frame")
    #expect_false(re$success)
  #  expect_equal(names(re), c("actorId", "xmlFormId", "roleId"))
#  })
  
  test_that("Can list all form assignments within a project ", {
    re = p_api$list_form_assignments_in_project(2)$content
    expect_s3_class(re, "data.frame")
    expect_equal(names(re), c("actorId", "xmlFormId", "roleId"))
  })
  
  
})