#' Response Class
#'
#' Response Class
#' @export
Response  = R6::R6Class(
  'Response',
  public = list(
    content = NULL,
    response = NULL,
    initialize = function(content, response){
      self$content = content
      self$response = response
    }
  ),
  active = list(
    #' @field message Error message, or NULL if there is none
    message = function(notused) {
      if (missing(notused)) {
        httr::content(self$response)$message
      } else {
        stop("message is read-only") # nocov
      }
    }
  )
)


