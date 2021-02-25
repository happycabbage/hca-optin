#' Receive opt-in data
#'
#' @param ... Form data
#'
#' @importFrom jsonlite toJSON fromJSON write_json
#' @importFrom httr POST status_code
#' @importFrom stringr str_glue
#' @importFrom data.table data.table
#' @importFrom pipelinetools dbconn dbdisconn
#' @importFrom DBI dbAppendTable
#' @importFrom lubridate now
#'
#' @name hca
NULL

#' @describeIn hca external form url
#' @export
hca <- function(...) {

  resp <- httr::POST(
    url = "localhost/ocpu/user/api/library/optin/R/store_session/json",
    encode = "json",
    body = list(ll = list(...)))

  status <- httr::status_code(resp)
  ts_gmt <- resp$date

  if (status == "201") {
    sid <- resp$headers$`x-ocpu-session`
    dat <- jsonlite::fromJSON(rawToChar(resp$content))
    loc <- stringr::str_glue("/ocpu/tmp/{sid}/files/{dat}")
  } else {
    sid <- NA_character_
    dat <- NA_character_
    loc <- NA_character_
  }
  row <- data.table::data.table(
    "status" = status,
    "ts_gmt" = ts_gmt,
    "session_id" = sid,
    "file_name" = dat,
    "get_path" = loc
  )

  cn <- hcaconnect::dbconn("127.0.0.1", db = "api", usr = "www-data")
  on.exit(hcaconnect::dbdisconn(cn))

  tryCatch({
    res <- DBI::dbAppendTable(cn, "optinsessions", row)
    stopifnot(res == 1)
  }, error = function(c) {f
    warning(c$message, call. = FALSE)
    return(invisible(FALSE))
  })
  return(invisible(TRUE))
}

#' @describeIn hca internal session url
store_session <- function(ll) {
  fnam <- paste0("formdata_", as.integer(lubridate::now()), ".json")
  jsonlite::write_json(ll, fnam)
  return(fnam)
}
