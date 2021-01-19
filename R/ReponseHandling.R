# Private helper functions
response_to_dataframe  = function(resp) {
  content = httr::content(resp)
  if (is.null(content) || length(content) == 0)
    return(NULL)
  is_nested = is.list(content[[1]])
  if (is_nested) {
    # Handle case of unequal columns
    cols = unique(unlist(sapply(content, names)))
    # Must remove NULL, otherwise problems with unequal number of columns
    content =
      lapply(content, function(z)
        lapply(z,  function(x)
          ifelse(is.null(x), NA, x)))
    return(do.call(rbind, lapply(content, function(x) {
      x <- data.frame(x)
      x[setdiff(cols, names(x))] <- NA
      x
    })))
  }
  content = replace(content, content == "NULL", NA)
  return(data.frame(content))
}

default_response = function(resp, valid_func, ...) {
  if (httr::status_code(resp) >= 200 &&
      httr::status_code(resp) <= 299) {
    valid_func(resp, ...)
  }  else if (httr::status_code(resp) >= 400 &&
              httr::status_code(resp) <= 499) {
    content = httr::content(resp)
    message = content$message
    if (grepl("details", message ) || length(content$details) > 0) {
      message = paste0(unlist(strsplit(message, "\\. "))[1], ". ")
      err = content$details$error
      warn = unlist(content$details$warnings)
      if (!is.null(err)) message = paste(message, "Error:", err)
      if (!is.null(warn)) message = paste(message, "Warning:", warn)
    }
    data.frame(success = FALSE, message = message) # nocov
  } else if (httr::status_code(resp) >= 500 &&
             httr::status_code(resp) <= 599) {
    data.frame(success = FALSE, message = "API server error")
  }
}

validate_file_path = function(form_id, filepath, extension ){
  if (is.null(filepath)) 
    stop("No output file name given") # nocov
  if (dir.exists(filepath)) {
    filepath = paste0(filepath, "/", form_id, ".", extension)
  } else {
    if (!dir.exists(dirname(filepath)))  
      stop("Directory ", dirname, "does not exist") # nocov
    ex = tools::file_ext(filepath)
    if (ex != extension) 
      stop("Extension of file must be ", extension) # nocov
  }
  normalizePath(filepath, mustWork = FALSE)
}

xml_to_dataframe = function(resp, ...){
  ct = xslt::xml_xslt(httr::content(resp), account$form_style, params = list())
  read.delim(textConnection(ct), header = TRUE)
}

write_file_func = function(resp, ... ) {
  args = list(...)
  outfile = args$outfile
  method = args$method  
  if (is.na(outfile)) 
    return(data.frame(success = TRUE, message = NA, filename = outfile))
  if (method == "character") {
    tryCatch(writeChar(as.character(httr::content(resp)), outfile), 
             error = function(e) return(
               data.frame(success = FALSE, message = e$message)))
  } else if (method == "binary") {
    tryCatch(writeBin(httr::content(resp, as = "raw"), outfile),
             error = function(e) return(
               data.frame(success = FALSE, message = e$message)))
  } else {
    stop("Method to write file must be character or binary, but is ", method) # nocov
  }
  data.frame(success = TRUE, message = NA, filename = outfile )
}


success_response = function(resp) {
  default_response(resp, function(resp)
    data.frame(success = TRUE, message = NA))
}

collapse_verbs_response = function(resp){
  content = httr::content(resp)
  # To simplify, we collapse the verbs
  # Are there multiple entries?
  is_nested = is.list(content[[1]])
  if (!is_nested) { # wrap in list
    content = list(content)      
  }
  for (i in 1:length(content)) {
    content[[i]]$verbs = paste(unlist(content[[i]]$verbs), collapse = ",")
    content[[i]] = lapply(content[[i]], function(x) ifelse(is.null(x), NA, x))
  }
  content = do.call(rbind, lapply(content, data.frame))
}

dataframe_response = function(resp, process_response) {
  if (missing(process_response))
    process_response = response_to_dataframe
  default_response(resp, process_response)
}

encode_instance_id = function(instance_id) {
  instance_id = URLencode(instance_id, reserved = TRUE)
}

# Code simplified from https://github.com/jsugarelli/flatxml
flattenXML = function(node, frame = NULL, path = character(0)) 
{
  if (is.null(frame)) 
    frame <- data.frame(elem = character(0), elemid = integer(0),  value = character(0))
  fixed.cols <- 3
  elem <- xml2::xml_name(node)
  if (nrow(frame) > 0) 
    elemid <- max(frame$elemid) + 1
  else elemid <- 0
  children <- xml2::xml_children(node)
  txt <- xml2::xml_find_first(node, "./text()")
  if (length(txt) > 0) 
    value <- as.character(txt)
  else value <- NA
  path <- append(path, elem)
  if (length(path) + fixed.cols > ncol(frame)) {
    x. <- rep(NA, nrow(frame))
    frame <- cbind(frame, x.)
    names(frame)[ncol(frame)] <- paste0("level", as.character(ncol(frame) - 
                                                                fixed.cols))
  }
  if (nrow(frame) > 0) {
    df <- frame[nrow(frame), ]
    df$elem[1] <- elem
    df$elemid[1] <- elemid
    df$value[1] <- value
    for (f in (fixed.cols + 1):ncol(frame)) df[, f] <- NA
    if (length(path) > 0) 
      for (f in 1:length(path)) df[, fixed.cols + f] <- path[f]
  }
  else {
    df <- data.frame(elem, elemid, value, level1 = path[1])
  }
  frame <- rbind(frame, df)
  if (length(children) > 0) {
    for (f in 1:length(children)) {
      if (regexpr("<", as.character(children[f])) != -1) 
        frame <- flattenXML(children[f], frame, path)
    }
  }
  rownames(frame) <- c()
  frame
}

get_extdata_file = function(filename) {
  ext_file =
    system.file("extdata", filename, package = "odkapi")
  if (ext_file == '')
    ext_file = rprojroot::find_package_root_file("inst", "extdata", filename)
  if (!file.exists(ext_file))
    stop("Package file ", ext_file, " not found")
  ext_file
}  

#nocov start
# Used by system backup only, which must be tested interactively
# therefore not included in coverage
json_body_to_dataframe = function(resp) {
  json = jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
  # Possible bad unnesting, mainly for get_current_configuration
  json = lapply(json, function(x){ if (length(x) == 0) NA else x })
  do.call(data.frame, json )
}
#nocov end

