# ODK Central API
#
# [ODK Central Backend](https://github.com/opendatakit/central-backend) is a RESTful API server that provides key functionality for creating and managing Open Data Kit data collection campaigns.
#
# OpenAPI spec version: 1.0
#
#' This class was auto generated by the swagger code generator program.
#' and modified by Dieter Menne
#' Ref: https://github.com/swagger-api/swagger-codegen
#'
#' @export
ApiClient  = R6::R6Class(
  'ApiClient',
  public = list(
    basePath = NULL,
    configuration = NULL,
    defaultHeaders = NULL,
    # extended by authorization with Bearer
    session = NULL,
    # Set by authorization
    timeout = 5,
    # in seconds
    retry_times = 1,
    
    initialize = function(basePath) {
      self$basePath = basePath
      self$defaultHeaders['Content-Type'] = "application/json"
      self$defaultHeaders['User-Agent'] = glue('dmenne-odkapi/{packageVersion("odkapi")}')
    },
    
    callApi = function(url,
                       method,
                       queryParams,
                       headerParams,
                       body,
                       ...) {
      headers =
        httr::add_headers(self$defaultHeaders, headerParams)
      timeout = httr::timeout(self$timeout)
      # cannot use missing here, because there is a base function body
      if (typeof(body) == "closure") 
        body = NULL
      ret = tryCatch({
        httr::RETRY(
          method,
          url,
          headers,
          body = body,
          query = queryParams,
          encode = "json",
          times = self$retry_times,
          timeout,
          ...
        )
      }, error = function(e) {
        if (length(grep("Timeout|resolve", e$message)) > 0)
          return(structure(
            .Data =
              list(
                status_code = 555,
                type = "",
                message = "Connection timeout"
              ),
            class = "response"
          ))
        else
          stop(e$message) # nocov
      })
      # Avoid rate limit problem with apiary server 
      # nocov start
      if (Sys.getenv("HTTPTEST_TYPE") != "mock") {
        limit = httr::headers(ret)$`x-apiary-ratelimit-remaining` 
        if (!is.null(limit) && as.integer(limit) < 10) {
          Sys.sleep(10) 
        } 
      }
      # nocov end
      ret
    }
  )
  
)
