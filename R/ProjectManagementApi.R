# ODK Central API
#
# [ODK Central Backend](https://github.com/opendatakit/central-backend) is a RESTful API server that provides key functionality for creating and managing Open Data Kit data collection campaigns.
#
# OpenAPI spec version: 1.0
# 

#' @title ProjectManagement operations
#'
#' @field apiClient Handles the client-server communication.
#'
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'
#' assign_actor_to_project_role Assigning an Actor to a Project Role
#' create_project Creating a Project
#' }
#'
#' @export
ProjectManagementApi = R6::R6Class(
  'ProjectManagementApi',
  public = list(
    userAgent = "odkapi",
    apiClient = NULL,
    
    initialize = function(apiClient){
      self$apiClient = apiClient
    },
    
#' Title
#'
#' @template project_id 
#' @param role_id 
#' @param actor_id 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
    assign_actor_to_project_role = function(project_id, role_id, actor_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/assignments/{role_id}/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = NULL,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },
    
    create_project = function(body, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    deep_update_project_and_form_details = function(project_id, body, ...){ 
      stop("Not implemented")
    },
    
    delete_project = function(project_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "DELETE",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = NULL,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },
    
    enable_project_managed_encryption = function(project_id, body,  ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/key"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },
    
    get_project_details = function(project_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = NULL,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    list_projects = function(...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },

    list_actors_assigned_project_role = function(project_id, role_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/assignments/{role_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    list_form_assignments_in_project = function(project_id,  ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/assignments/forms"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    list_role_specific_form_assignments_in_project = function(project_id, role_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/assignments/forms/{role_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    list_project_assignments = function(project_id,  ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/assignments"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "GET",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = NULL,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    revoke_project_role_assignment_from_actor = function(project_id, role_id, actor_id,  ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/assignments/{role_id}/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "DELETE",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp) 
    },

    update_project_details = function(project_id, body,  ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                 method = "PATCH",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    }
  )
)
