# ODK Central API
#
# [ODK Central Backend](https://github.com/opendatakit/central-backend) is a RESTful API server that provides key functionality for creating and managing Open Data Kit data collection campaigns.
#
# OpenAPI spec version: 1.0
#

#' @title Authentication operations
#' @description swagger.Authentication
#'
#' @field path Stores url path of the request.
#' @field apiClient Handles the client-server communication.
#'
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'
#' login Logging in.  When  no email or password is given, these are retrieved
#' from the environment by  `email = Sys.getenv("ODKC_UN")`  and `password = Sys.getenv("ODKC_PW")`.
#' See package ruODK for details of the environment variables
#'
#' logout Logging out using token or default token from login
#'
#'
#' use_app_user_authentication Using App User Authentication
#'
#' }
#'
#' @export
AuthenticationApi = R6::R6Class(
  'AuthenticationApi',
  public = list(
    apiClient = NULL,
    initialize = function(apiClient) {
        self$apiClient = apiClient
    },
    login = function(email, password, ...) {
      if (missing(email)) {
        email = Sys.getenv("ODKC_UN")
      }
      if (missing(password))
        password = Sys.getenv("ODKC_PW")
      headerParams = character()
      body =  list(email = email, password = password)
      # Very important, otherwise 401 on multiple logins!
      httr::handle_reset(self$apiClient$basePath) 
      
      urlPath = "{self$apiClient$basePath}/v1/sessions"
      resp =
        self$apiClient$callApi(
          url = glue(urlPath),
          method = "POST",
          queryParams = list(),
          headerParams = headerParams,
          body = body,
          handle = NULL,
          ...
        )
      returnObject = dataframe_response(resp)
      # Keep copy of session to use bearer in future
      self$apiClient$defaultHeaders =
        replace(
          self$apiClient$defaultHeaders,
          "Authorization",
          paste0("Bearer ", returnObject$token)
        )
      self$apiClient$session = returnObject
      Response$new(returnObject, resp)
    },
    logout = function(...) {
      headerParams = character()
      if (is.null(self$apiClient)) {
        content = data.frame(success = FALSE, 
             message = "Trying to log out without login")
        return(Response$new(content, NULL))
      }
      headerParams = self$apiClient$defaultHeaders
      # Do not remove 'token' even if there is an info
      token = URLencode(self$apiClient$session$token, reserved = TRUE)
      # Reset: Very important, otherwise 401 on multiple logins!
      httr::handle_reset(self$apiClient$basePath)
      urlPath =  "{self$apiClient$basePath}/v1/sessions/{token}"
      resp = self$apiClient$callApi(
        url = glue(urlPath),
        method = "DELETE",
        queryParams = list(),
        headerParams = headerParams,
        body = NULL,
        ...
      )
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    }
  )
)
