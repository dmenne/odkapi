# ODK Central API
#
# [ODK Central Backend](https://github.com/opendatakit/central-backend) is a RESTful API server that provides key functionality for creating and managing Open Data Kit data collection campaigns.
#
# OpenAPI spec version: 1.0
# 

#' @title SystemEndpoints operations
#'
#' @field path Stores url path of the request.
#' @field apiClient Handles the client-server communication.
#'
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'
#'
#' }
#'
#' @export
SystemEndpointsApi = R6::R6Class(
  'SystemEndpointsApi',
  public = list(
    apiClient = NULL,
    initialize = function(apiClient){
      self$apiClient = apiClient
    },
    
    # Audit logs ---------------    
    get_audit_log_entries = function(action, start, end, limit, offset, ...){
      queryParams = list()
      headerParams = character()
      if (!missing(`action`)) queryParams['action'] = action
      if (!missing(`start`))  queryParams['start'] = start
      if (!missing(`end`))    queryParams['end'] = end
      if (!missing(`limit`))  queryParams['limit'] = limit
      if (!missing(`offset`)) queryParams['offset'] = offset
      urlPath = "{self$apiClient$basePath}/v1/audits"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp)
    },
    
    # Google backup ---------------    
    complete_new_backup_configuration = function(token, google_code,  ...){
      queryParams = list()
      headerParams = character()
      headerParams["Authorization"] = glue("Bearer {token}")
      body = list(code = google_code)
      urlPath = "{self$apiClient$basePath}/v1/config/backups/verify"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    get_current_configuration = function(...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/config/backups"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp, json_body_to_dataframe)
      Response$new(returnObject, resp)
    },
    
    initiate_new_backup_configuration = function(passphrase, ...){
      queryParams = list()
      headerParams = character()
      body = list(passphrase = passphrase)
      urlPath = "{self$apiClient$basePath}/v1/config/backups/initiate"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp,json_body_to_dataframe)
      Response$new(returnObject, resp)
    },
    
    terminate_current_configuration = function(...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/config/backups"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "DELETE",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    }
  )
)
