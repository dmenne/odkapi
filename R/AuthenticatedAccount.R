#' Factory to create an account and log in
#'
#' @param url 
#' @param email 
#' @param password 
#'
#' @return A account with a logged in user
#' @export
#'
AuthenticatedAccount = function(url, email = NULL, password = NULL) {
  if (is.null(email)) email = Sys.getenv("ODKC_UN")
  if (is.null(password)) password = Sys.getenv("ODKC_PW")
  if (is.null(email) || is.null(password))  
    stop("No email or password") # nocov
  if (!can_use_server(url)) 
    stop("Cannot connect to ", url) # nocov
  api_client = ApiClient$new(basePath = url)
  authentication = AuthenticationApi$new(api_client)
  account = AccountsAndUsersApi$new(api_client, authentication)
  login = authentication$login(email, password)
  if (!is.null(login$message))
    stop("Login not successful: ", login$message) # nocov
  account
}

#' @export
can_use_server = function(url) {
  # Listing projects does not require login
  # Use it to check for server timeout
  use = tryCatch(httr::GET(glue::glue("{url}/v1/projects"), httr::timeout(1)), 
                 error = function(e) "timeout")
  !(is.character(use)  ||  use$status_code != 200)
}


