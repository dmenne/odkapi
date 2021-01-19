# ODK Central API
#
# [ODK Central Backend](https://github.com/opendatakit/central-backend) is a RESTful API server that provides key functionality for creating and managing Open Data Kit data collection campaigns.
#
# OpenAPI spec version: 1.0
#' @title AccountsAndUsers operations
#' @description swagger.AccountsAndUsers
#'
#' @field apiClient Handles the client-server communication.
#'
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'
#' }
#'
#' @export
AccountsAndUsersApi = R6::R6Class(
  'AccountsAndUsersApi',
  public = list(
    apiClient = NULL,
    authentication = NULL,
    initialize = function(apiClient, authentication){
      if (!missing(apiClient))  self$apiClient = apiClient
      if (!missing(authentication))  self$authentication = authentication
    },
    
# Non-REST helper functions --------------------
    project_id_from_name = function(name){
      p_api = ProjectManagementApi$new(self$apiClient)
      projects = p_api$list_projects()$content 
      project_id = projects[projects$name == name & is.na(projects$archived),"id"]
      if (length(project_id) == 0) return(NULL)
      project_id
    },

# REST functions --------------------    
    assign_actor_to_server_wide_role = function(role_id, actor_id,  ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/assignments/{role_id}/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = NULL, # Not required
                                 ...)
      
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },
    
    create_new_app_user = function(project_id, body, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/app-users"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    create_new_user = function(body, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/users"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    delete_app_user = function(project_id, id, ...){
      queryParams = list()
      headerParams = character()

      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/app-users/{id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "DELETE",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },
    
    delete_user = function(actor_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/users/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "DELETE",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },
    
    directly_update_user_password = function(actor_id, body, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/users/{actor_id}/password"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "PUT",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },
    
    get_role_details = function(id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/roles/{id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)    
      returnObject = dataframe_response(resp, collapse_verbs_response)
      Response$new(returnObject, resp) 
    },
    
    get_user_details = function(actor_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/users/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp)       
    },
    
    get_authenticated_user_details = function(...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/users/current"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    initiate_password_reset = function(body, invalidate, ...){
      queryParams = list()
      headerParams = character()
      if (!missing(invalidate))
        queryParams['invalidate'] = invalidate
      urlPath = "{self$apiClient$basePath}/v1/users/reset/initiate"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },
    
    list_actors_assigned_role = function(role_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/assignments/{role_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp)  
    },
    
    list_app_users = function(project_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/app-users"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    list_assignments = function(...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/assignments"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    list_roles = function(...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/roles"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp, collapse_verbs_response)
      Response$new(returnObject, resp) 
    },
    
    list_users = function(q, ...){
      # Version 1.0: q does not work
      queryParams = list()
      headerParams = character()
      if (!missing(`q`)) {
        queryParams['q'] = q
      }
      urlPath = "{self$apiClient$basePath}/v1/users"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    modify_user = function(actor_id, body, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/users/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "PATCH",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    strip_role_assignment_from_actor = function(role_id, actor_id,  ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/assignments/{role_id}/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "DELETE",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = NULL,
                                 ...)
      
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },

  # Higher level functions using REST API --------------------

  #' @description
  #' Returns App user qr code
  #' \url{https://docs.getodk.org/collect-import-export/#list-of-keys-for-all-settings}
  #'
  #' @param project_id The project ID
  #' @param user_id The id of the user, e.g. 115
  #' @param show_image Displays the QR-Code with image
  #' @param collect_json JSON file in inst/extdata as template for settings. Use **
  #' to delimit `draft_token` insert location.
  #' @return QR code for display with `image`
  #' @export
  app_user_qr_code = function(project_id,
                              user_id,
                              show_image = FALSE,
                              collect_json = "app_user_settings.json") {
    app_users = self$list_app_users(project_id)$content
    if (!is.null(app_users$message)) 
      stop("app_user_qr_code: Invalid project id")
    if (!(user_id %in% app_users$id))
        stop("app_user_qr_code: Invalid user id")
    key = app_users$token # Do not remove, used in glue
    # Settings file must use ** to delimit replaceable tokens  to avoid
    # collisions with braces in json
    settings_file =  get_extdata_file(collect_json)
    stopifnot(file.exists(settings_file))
    # Read settings with ** as glue delimiter
    # TODO : remove readr!
    settings = gsub("[\r\n\t ]", "",  readr::read_file(settings_file))
    qq = glue::glue(settings, .open = "**", .close = "**")
    
    # Line breaks must be removed. qrencode does not follow standards here
    # https://github.com/jeroen/jsonlite/issues/220
    q64 = gsub("[\r\n]", "", jsonlite::base64_enc(memCompress(qq, "gzip")))
    qr = qrencoder::qrencode_raw(q64)
    if (show_image) {
      # pp = par(mar = c(0,0,0,0) )
      image(qr,  asp = 1, col = c("white", "black"), axes = FALSE,  
            useRaster = TRUE,
            xlab = glue::glue(
              "Draft: Project ({project_id}), user_id: {user_id}"), ylab = "")
      # par(pp)
    }
    invisible(qr)
  }
  ),
  # active fields
  active = list(    
    form_style = function(value) {
      if (missing(value)) {
        if (is.null(private$.form_style)) {
          file = private$get_extdata_file("odkapi_form.xsl")
          stopifnot(file.exists(file))
          private$.form_style = xml2::read_xml(file)
        }
        private$.form_style
      } else {
        private$.form_style = value
      }
    },
    languages_style = function(value) {
      if (missing(value)) {
        if (is.null(private$.languages_style)) {
          file = private$get_extdata_file("odkapi_languages.xsl")
          stopifnot(file.exists(file))
          private$.languages_style = xml2::read_xml(file)
        }
        private$.languages_style
      } else {
        private$.languages_style = value
      }
    },
    choices_style = function(value) {
      if (missing(value)) {
        if (is.null(private$choices_style)) {
          file = private$get_extdata_file("odkapi_choices.xsl")
          stopifnot(file.exists(file))
          private$.choices_style = xml2::read_xml(file)
        }
        private$.choices_style
      } else {
        private$.choices_style = value
      }
    }
  ),
  # private functions -----------------
  private = list(
    # XML of xslt transformation to retrieve form, languages and 
    # choices as a data frame.
    .form_style = NULL,
    .languages_style = NULL,
    .choices_style = NULL,
    get_extdata_file = function(filename) {
      ext_file =
        system.file("extdata", filename, package = "odkapi")
      if (ext_file == '')
        ext_file = rprojroot::find_package_root_file("inst", "extdata", filename)
      if (!file.exists(ext_file))
        stop("Package file ", ext_file, " not found")
      ext_file
    }
)

)
