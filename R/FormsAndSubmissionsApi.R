# ODK Central API
#
# [ODK Central Backend](https://github.com/opendatakit/central-backend) is a RESTful API server that provides key functionality for creating and managing Open Data Kit data collection campaigns.
#
# OpenAPI spec version: 1.0
#

#' @title FormsAndSubmissions operations
#' @description swagger.FormsAndSubmissions
#'
#' @field path Stores url path of the request.
#' @field apiClient Handles the client-server communication.
#'
#' @importFrom R6 R6Class
#' @importFrom glue glue
#'
#' @section Methods:
#' \describe{
#'
#' assign_actor_to_form_role Assigning an Actor to a Form Role
#' }
#'
#' @export
FormsAndSubmissionsApi = R6::R6Class(
  'FormsAndSubmissionsApi',
  public = list(
    apiClient = NULL,
    initialize = function(apiClient){
      self$apiClient = apiClient
    },
    
    assign_actor_to_form_role = function(project_id, xml_form_id, role_id, actor_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/assignments/{role_id}/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    
    clear_form_attachment = function(project_id, xml_form_id, filename, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "DELETE",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    
    clear_draft_submission_attachment = function(project_id, xml_form_id, 
                                                 instance_id, filename, ...){
      queryParams = list()
      headerParams = character()
      instance_id = encode_instance_id(instance_id)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions/{instance_id}/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "DELETE",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    
   clear_submission_attachment = function(project_id, xml_form_id, instance_id, filename,  ...){
      queryParams = list()
      headerParams = character()
      instance_id = encode_instance_id(instance_id)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions/{instance_id}/attachments/{basename(filename)}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "DELETE",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    
    create_draft_form = function(project_id, xml_form_id,  filename, 
                                 x_xls_form_form_id_fallback, ignore_warnings, ...){
      queryParams = list()
      headerParams = character()
      headerParams['Content-Type'] = mime::guess_type(filename)
      if (!missing(`x_xls_form_form_id_fallback`)) {
        headerParams['X-XlsForm-FormId-Fallback'] = `x_xls_form_form_id_fallback`
      } else {
        headerParams['X-XlsForm-FormId-Fallback'] =  
          basename(tools::file_path_sans_ext(filename))
      }
      if (!missing(`ignore_warnings`)) 
        queryParams['ignoreWarnings'] = ignore_warnings
      if (!file.exists(filename))
        stop("File ", filename, " does not exits")
      body = httr::upload_file(filename)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
   create_draft_submission = function(project_id, xml_form_id, body, ...){
     queryParams = list()
     headerParams = character()
     headerParams["Content-Type"] =  "application/xml"
     urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions"
     if (substr(body, 1, 1) != "<") {
       if (!file.exists(body)) {
         stop("Draft submission file ", body, " does not exist")
       }
       body = httr::upload_file(body)
     }
     resp = self$apiClient$callApi(url = glue(urlPath),
                                   method = "POST",
                                   queryParams = queryParams,
                                   headerParams = headerParams,
                                   body = body,
                                   ...)
     returnObject = dataframe_response(resp)
     Response$new(returnObject, resp) 
   },
   
    create_form = function(project_id, filename,  x_xls_form_form_id_fallback,
                           ignore_warnings, publish, ...){
      queryParams = list()
      headerParams = character()
      headerParams['Content-Type'] = mime::guess_type(filename)
      
      if (!missing(`x_xls_form_form_id_fallback`)) {
        headerParams['X-XlsForm-FormId-Fallback'] = `x_xls_form_form_id_fallback`
      } else {
        headerParams['X-XlsForm-FormId-Fallback'] =  
          basename(tools::file_path_sans_ext(filename))
      }
      if (!missing(`ignore_warnings`)) 
        queryParams['ignoreWarnings'] = ignore_warnings
      if (!missing(`publish`)) 
        queryParams['publish'] = publish
      if (!file.exists(filename))
        stop("File ", filename, " does not exits")
      body = httr::upload_file(filename)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    create_link = function(project_id, xml_form_id, body, once,  ...){
      queryParams = list()
      headerParams = character()
      if (!missing(once))
        queryParams["once"] = once
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/public-links"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
      
    },
    

    create_submission = function(project_id, xml_form_id,  body, ...){
      queryParams = list()
      headerParams = character()
      headerParams["Content-Type"] = "application/xml"
      if (substr(body, 1, 1) != "<") {
        if (!file.exists(body)) {
          stop("Draft submission file ", body, " does not exist")
        }
        body = httr::upload_file(body)
      }
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    delete_draft_form = function(project_id, xml_form_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "DELETE",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    
    delete_form = function(project_id, xml_form_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "DELETE",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    
    delete_link = function(project_id, xml_form_id, link_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/public-links/{link_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "DELETE",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    download_draft_submission_attachment = function(project_id, xml_form_id, 
                                         instance_id, filename, outdir = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      instance_id = encode_instance_id(instance_id)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions/{instance_id}/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = file.path(outdir, filename))
      Response$new(returnObject, resp)
    },
    
    
    
    download_draft_form_attachment = function(project_id, 
                           xml_form_id, filename, outdir = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = file.path(outdir, filename))
      Response$new(returnObject, resp)
    },
    
    
    download_form_attachment = function(project_id, xml_form_id, filename, 
                                        outdir = tempdir(), ...){
      queryParams = list()
      headerParams = c("Content-Type" = mime::guess_type(filename),
                       "Content-Disposition" = paste0("attachment; filename=", filename))
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = file.path(outdir, filename))
      Response$new(returnObject, resp)
    },
    
    download_form_version_attachment = function(project_id, xml_form_id, version, filename, 
                                                outdir = tempdir(), ...){
      queryParams = list()
      headerParams = c("Content-Type" = mime::guess_type(filename),
                       "Content-Disposition" = paste0("attachment; filename=", filename))
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/versions/{version}/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = file.path(outdir, filename))
      Response$new(returnObject, resp)
    },
    
    
    download_submission_attachment = function(project_id, xml_form_id, instance_id, filename, 
                                              outdir = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      headerParams = c("Content-Type" = mime::guess_type(filename))
      instance_id = encode_instance_id(instance_id)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions/{instance_id}/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = file.path(outdir, filename))
      Response$new(returnObject, resp)
    },
    
    
    export_draft_submissions_to_csv_via_post = function(project_id, xml_form_id, 
                                                        outfile = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions.csv.zip"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      outfile = validate_file_path(xml_form_id, outfile , "zip")
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = outfile)
      Response$new(returnObject, resp)
      
    },
    
    
    export_draft_submissions_to_csv = function(project_id, xml_form_id, 
                                               outfile = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions.csv.zip"
      outfile = validate_file_path(xml_form_id, outfile , "zip")
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = outfile)
      Response$new(returnObject, resp)
    },
    
    
    export_submissions_to_csv_via_post = function(project_id, xml_form_id, 
                                                       outfile = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions.csv.zip"
      outfile = validate_file_path(xml_form_id, outfile , "zip")
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = outfile)
      Response$new(returnObject, resp)
    },
    
    
    export_submissions_to_csv = function(project_id, xml_form_id, 
                                              outfile = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions.csv.zip"
      outfile = validate_file_path(xml_form_id, outfile , "zip")
      
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      outfile = outfile)
      Response$new(returnObject, resp)
    },
    
    
    get_draft_form_details = function(project_id, xml_form_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    get_draft_form_schema_fields = function(project_id, xml_form_id, odata=FALSE, ...){
      queryParams = list()
      headerParams = character()
      queryParams['odata'] = odata
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/fields"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    get_draft_submission_details = function(project_id, xml_form_id, instance_id, ...){
      queryParams = list()
      headerParams = character()
      instance_id = encode_instance_id(instance_id)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions/{instance_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    get_form_details = function(project_id, xml_form_id,  ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    get_form_schema_fields = function(project_id, xml_form_id,odata = FALSE, ...){
      queryParams = list()
      headerParams = character()
      queryParams['odata'] = odata
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/fields"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    get_form_version_details = function(project_id, xml_form_id, version, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/versions/{version}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    get_form_version_schema_fields = function(project_id, xml_form_id, version, 
                                              odata=FALSE, ...){
      queryParams = list()
      headerParams = character()
      queryParams['odata'] = odata
      if (!missing(`odata`)) {
        queryParams['odata'] = odata
      }
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/versions/{version}/fields"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    get_submission_details = function(project_id, xml_form_id, instance_id, ...){
      queryParams = list()
      headerParams = character()
      instance_id = encode_instance_id(instance_id)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions/{instance_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
   list_actors_assigned_form_role = function(project_id, xml_form_id, role_id, ...){
     queryParams = list()
     headerParams = character()
     urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/assignments/{role_id}"
     resp = self$apiClient$callApi(url = glue(urlPath),
                                   method = "GET",
                                   queryParams = queryParams,
                                   headerParams = headerParams,
                                   body = body,
                                   ...)
     returnObject = dataframe_response(resp)
     Response$new(returnObject, resp) 
   },
   
   
   list_all_forms = function(project_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
   
   list_draft_form_attachments = function(project_id, xml_form_id, ...){
     queryParams = list()
     headerParams = character()
     urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/attachments"
     resp = self$apiClient$callApi(url = glue(urlPath),
                                   method = "GET",
                                   queryParams = queryParams,
                                   headerParams = headerParams,
                                   body = body,
                                   ...)
     returnObject = dataframe_response(resp)
     Response$new(returnObject, resp) 
   },
   
   
   list_draft_submissions_encryption_keys = function(project_id, xml_form_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions/keys"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    list_encryption_keys = function(project_id, xml_form_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions/keys"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    

   list_expected_draft_submissions_attachments =  
     function(project_id,  xml_form_id, instance_id, ...){
       queryParams = list()
       headerParams = character()
       instance_id = encode_instance_id(instance_id)
       urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions/{instance_id}/attachments"
       resp = self$apiClient$callApi(url = glue(urlPath),
                                     method = "GET",
                                     queryParams = queryParams,
                                     headerParams = headerParams,
                                     body = body,
                                     ...)
       returnObject = dataframe_response(resp)
       Response$new(returnObject, resp) 
     },
   
   
   list_expected_submission_attachments = function(project_id, xml_form_id, 
                                                   instance_id,  ...){
     queryParams = list()
     headerParams = character()
     instance_id = encode_instance_id(instance_id)
     urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions/{instance_id}/attachments"
     resp = self$apiClient$callApi(url = glue(urlPath),
                                   method = "GET",
                                   queryParams = queryParams,
                                   headerParams = headerParams,
                                   body = body,
                                   ...)
     returnObject = dataframe_response(resp)
     Response$new(returnObject, resp) 
   },
   
   
   list_form_assignments = function(project_id, xml_form_id, ...){
     queryParams = list()
     headerParams = character()
     urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/assignments"
     resp = self$apiClient$callApi(url = glue(urlPath),
                                   method = "GET",
                                   queryParams = queryParams,
                                   headerParams = headerParams,
                                   body = body,
                                   ...)
     returnObject = dataframe_response(resp)
     Response$new(returnObject, resp) 
   },

      
    list_form_attachments = function(project_id, xml_form_id,...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/attachments"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    list_form_version_attachments = function(project_id, xml_form_id, version, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/versions/{version}/attachments"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
   list_links = function(project_id, xml_form_id, ...){
     queryParams = list()
     headerParams = character()
     urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/public-links"
     resp = self$apiClient$callApi(url = glue(urlPath),
                                   method = "GET",
                                   queryParams = queryParams,
                                   headerParams = headerParams,
                                   body = body,
                                   ...)
     returnObject = dataframe_response(resp)
     Response$new(returnObject, resp) 
   },
   
   
   list_published_form_versions = function(project_id, xml_form_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/versions"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    list_submissions_on_draft_form = function(project_id, xml_form_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    list_submissions_on_form = function(project_id, xml_form_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = dataframe_response(resp)
      Response$new(returnObject, resp) 
    },
    
    
    modify_form = function(project_id, xml_form_id, body, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "PATCH",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    publish_draft_form = function(project_id, xml_form_id, version, auto_version = FALSE, ...){
      queryParams = list()
      headerParams = character()
      if (auto_version) {
        version = paste(xml_form_id,
          tolower(sample(readLines(get_extdata_file("first_names.txt")),1)), sep = "_")
      }
      if (!missing(version))
        queryParams['version'] = version
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/publish"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    retrieve_draft_form_xlsx_file = function(project_id, xml_form_id, 
                                        xlsxpath = tempdir(), ...){
      queryParams = list()
      headerParams = c("Content-Type" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft.xlsx"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      outfile = validate_file_path(xml_form_id, xlsxpath, "xlsx")
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      form_id = xml_form_id, 
                                      outfile = outfile, 
                                      extension = "xlsx")
      Response$new(returnObject, resp)
    },
    
    
   retrieve_draft_form_xml_df = function(project_id, xml_form_id, ...){
     queryParams = list()
     headerParams = character()
     headerParams = c(`Content-Type` = "application/xml", 
                      `Accept` = "application/xml")
     urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft.xml"
     resp = self$apiClient$callApi(url = glue(urlPath),
                                   method = "GET",
                                   queryParams = queryParams,
                                   headerParams = headerParams,
                                   body = body,
                                   ...)
     returnObject = default_response(resp, xml_to_dataframe)
     Response$new(returnObject, resp)
   },
   
   
   retrieve_draft_form_xml_file = function(project_id, xml_form_id, xmlpath = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      headerParams = c(`Content-Type` = "application/xml", 
                       `Accept` = "application/xml")
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft.xml"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      # Save to file
      outfile = validate_file_path(xml_form_id, xmlpath, "xml")
      returnObject = default_response(resp, write_file_func,
                                      method = "binary", # Inconsistent. Gives UTF-warning on character
                                      outfile = outfile)
      Response$new(returnObject, resp)
    },
    
    
    retrieve_form_version_xlsx_file = function(project_id, xml_form_id, version,
                                          xlsxpath = tempdir(), ...){
      queryParams = list()
      headerParams = c("Content-Type" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/versions/{version}.xlsx"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      outfile = validate_file_path(xml_form_id, xlsxpath, "xlsx")
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      form_id = xml_form_id, 
                                      outfile = outfile, 
                                      extension = "xlsx")
      Response$new(returnObject , resp)
    },
    
    
    retrieve_form_version_xml_file = function(project_id, xml_form_id, version, 
                                         xmlpath= tempdir(),  ...){
      queryParams = list()
      headerParams = c(`Content-Type` = "application/xml", 
                       `Accept` = "application/xml")
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/versions/{version}.xml"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      # Save to file
      outfile = validate_file_path(xml_form_id, xmlpath, "xml")
      returnObject = default_response(resp, write_file_func,
                                      method = "character",
                                      outfile = outfile)
      Response$new(returnObject, resp)
    },
    
    
    retrieve_draft_submission_xml = function(project_id, xml_form_id, 
                              instance_id, xmlpath = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      instance_id = encode_instance_id(instance_id)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions/{instance_id}.xml"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      # Save to file
      outfile = validate_file_path(xml_form_id, xmlpath, "xml")
      returnObject = default_response(resp, write_file_func,
                                      method = "character",
                                      outfile = outfile)
      Response$new(returnObject, resp)
    },
    
    retrieve_form_xlsx = function(project_id, xml_form_id, xlsxpath = tempdir(), ...) {
      queryParams = list()
      headerParams = c("Content-Type" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}.xlsx"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      outfile = validate_file_path(xml_form_id, xlsxpath, "xlsx")
      returnObject = default_response(resp, write_file_func,
                                      method = "binary",
                                      form_id = xml_form_id, 
                                      outfile = outfile, 
                                      extension = "xlsx")
      Response$new(returnObject , resp)
    },
    
    
    retrieve_form_xml_file = function(project_id, xml_form_id, xmlpath = tempdir(), ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}.xml"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body, ...)
      # Save to file
      outfile = validate_file_path(xml_form_id, xmlpath, "xml")
      returnObject = default_response(resp, write_file_func,
                                      method = "character",
                                      outfile = outfile)
      Response$new(returnObject, resp)
    },
    
   retrieve_form_xml_df = function(project_id, xml_form_id, xmlpath = tempdir(), ...){
     queryParams = list()
     headerParams = character()
     urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}.xml"
     resp = self$apiClient$callApi(url = glue(urlPath),
                                   method = "GET",
                                   queryParams = queryParams,
                                   headerParams = headerParams,
                                   body = body, ...)
     returnObject = default_response(resp, xml_to_dataframe)
     Response$new(returnObject, resp)
   },
   
   
   
    retrieve_submission_xml = function(project_id, xml_form_id, instance_id, 
                                       xmlpath, ...){
      queryParams = list()
      headerParams = character()
      instance_id = encode_instance_id(instance_id)
      if (missing(xmlpath)) xmlpath = tempdir()
        
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions/{instance_id}.xml"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "GET",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      # Save to file if not NA
      if (!is.na(xmlpath))
        outfile = validate_file_path(xml_form_id, xmlpath, "xml")
      else 
        outfile = NA
      returnObject = default_response(resp, write_file_func,
                                      method = "character",
                                      outfile = outfile)
      Response$new(returnObject, resp)
    },
    
   retrieve_submission_flattened = function(project_id, xml_form_id, instance_id, ...) {
     submission = self$retrieve_submission_xml(project_id, xml_form_id, instance_id, NA)
     returnObject = flattenXML(xml2::read_xml(submission$response$content))
     # Column "level1" is redundant, always "data"
     returnObject = returnObject[,-4]  
     Response$new(returnObject, submission$response)
   },  
       
    revoke_form_role_assignment_from_actor = function(project_id, xml_form_id, role_id, actor_id, ...){
      queryParams = list()
      headerParams = character()
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/assignments/{role_id}/{actor_id}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "DELETE",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    upload_draft_submission_attachment = function(project_id, xml_form_id, 
                    instance_id, filename, upload_file_path, ...){
      queryParams = list()
      headerParams = character()
      headerParams = c("Content-Type" = mime::guess_type(filename))
      instance_id = encode_instance_id(instance_id)
      if (!file.exists(upload_file_path)) 
        stop("Attachment file '", filename, "' does not exist")
      body = readBin(upload_file_path, "raw", file.info(upload_file_path)$size)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/submissions/{instance_id}/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
    
    upload_draft_form_attachment = function(project_id, xml_form_id, filename,  ...){
      queryParams = list()
      headerParams = c("Content-Type" = mime::guess_type(filename))
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/draft/attachments/{basename(filename)}"
      body = httr::upload_file(filename)
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
   
    upload_submission_attachment = function(project_id, xml_form_id, instance_id, filename, 
                                            upload_file_path, ...){
      queryParams = list()
      headerParams = c("Content-Type" = mime::guess_type(filename))
      instance_id = encode_instance_id(instance_id)
      if (!file.exists(upload_file_path)) 
        stop("Attachment file '", upload_file_path, "' does not exist")
      body = httr::upload_file(upload_file_path)
      urlPath = "{self$apiClient$basePath}/v1/projects/{project_id}/forms/{xml_form_id}/submissions/{instance_id}/attachments/{filename}"
      resp = self$apiClient$callApi(url = glue(urlPath),
                                    method = "POST",
                                    queryParams = queryParams,
                                    headerParams = headerParams,
                                    body = body,
                                    ...)
      returnObject = success_response(resp)
      Response$new(returnObject, resp)
    },
   
   #' @description
   #' Returns draft QR Code.
   #' \url{https://docs.getodk.org/collect-import-export/#list-of-keys-for-all-settings}
   #'
   #' @param project_id The project ID of the form
   #' @param xml_form_id The id of this Form as given in its XForms XML
   #' @param draft_token as obtained from `ru_draft_form_details`
   #' @param show_image Displays the QR-Code with image
   #' @param collect_json JSON file in inst/extdata as template for settings. Use **
   #' to delimit `draft_token` insert location.
   #' @return QR code for display with `image`
   #' @export
   draft_qr_code = function(project_id,
                            xml_form_id, 
                            draft_token,
                            show_image = FALSE,
                            collect_json = "collect_settings.json") {
     # Settings file must use ** to delimit replaceable tokens  to avoid
     # collisions with braces in json
     settings_file =  get_extdata_file(collect_json)
     # Do not remove server_url, it is used in {} replacement
     server_url = glue::glue("{self$apiClient$basePath}/v1/test/{draft_token}/projects/{project_id}/forms/{xml_form_id}/draft")
     # Read settings with ** as glue delimiter
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
               "Draft: Project ({project_id}), Form: {xml_form_id}"), ylab = "")
       # par(pp)
     }
     invisible(qr)
   }
  )
)
