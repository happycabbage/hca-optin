#' Receive opt-in data
#'
#' @param ... Form data
#'
#' @importFrom jsonlite toJSON fromJSON write_json
#' @importFrom httr POST status_code
#' @importFrom stringr str_glue str_extract
#' @importFrom data.table data.table
#' @importFrom hcaconnect dbconn dbdisconn
#' @importFrom DBI dbAppendTable
#' @importFrom lubridate now
#'
#' @name hca
NULL


#' @describeIn hca external form url
postData <- function(...) {
  r <- httr::POST(
    "localhost/ocpu/user/api/library/optin/R/store_session/json",
    encode = "json",
    body = list(data = data.table::data.table(...))
  )
  return(r)
}


#' @describeIn hca external form url
#' @export
hca <- function(...) {
  r <- postData(...)
  
  if (r$status == "201") {
    tmp <- httr::content(r, "parsed")[[1]]
    sid <- r$headers$`x-ocpu-session`
    loc <- stringr::str_glue("/ocpu/tmp/{sid}/files/")
  } else {
    tmp <- NA_character_
    sid <- NA_character_
    loc <- httr::content(r, as = "text")
  }
  row <- data.table::data.table(
    "status" = r$status,
    "ts_gmt" = lubridate::with_tz(lubridate::now(), "GMT"),
    "session_id" = sid,
    "file_name" = tmp,
    "get_path" = loc
  )

  
  cn <- DBI::dbConnect(RPostgres::Postgres(), 
                       host = "127.0.0.1", 
                       db = "api", 
                       usr = "www-data")
  on.exit(DBI::dbDisconnect(cn))

  tryCatch({
    res <- DBI::dbAppendTable(cn, "optinsessions", row)
    stopifnot(res == 1)
  }, error = function(c) {
    warning(c$message, call. = FALSE)
    return(invisible(FALSE))
  })
  return(invisible(TRUE))
}

#' @describeIn hca internal session url
store_session <- function(data) {
  tmp <- tempfile(tmpdir = "", pattern = "form", fileext = ".fst")
  fp <- stringr::str_extract(tmp, "(?<=/).+")
  fst::write_fst(data, fp)
  ##
  ## For now do nothing with the form data
  ##
  invisible(fp)
}
