#' Receive opt-in data
#'
#' @param ... Form data
#'
#' @importFrom jsonlite toJSON fromJSON write_json
#' @importFrom httr POST status_code
#' @importFrom stringr str_glue
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
    sid <- r$headers$`x-ocpu-session`
    loc <- stringr::str_glue("/ocpu/tmp/{sid}/files/formdata.fst")
  } else {
    sid <- NA_character_
    loc <- httr::content(r, as = "text")
  }
  row <- data.table::data.table(
    "status" = r$status,
    "ts_gmt" = r$date,
    "session_id" = sid,
    "file_name" = "formdata.fst",
    "get_path" = loc
  )

  cn <- hcaconnect::dbconn("127.0.0.1", db = "api", usr = "www-data")
  on.exit(hcaconnect::dbdisconn(cn))

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
  
  fst::write_fst(data, paste0(tempdir(), "formdata.fst"))
  ##
  ## For now do nothing with the form data
  ##
  invisible(TRUE)
}

# ll <- jsonlite::fromJSON('{"name":["sadfgasdf"],"Phone":["12341245"],"Do-you-consent":["Yes"],"org":["goldflora"]}')



