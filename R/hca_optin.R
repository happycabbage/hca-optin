#' Receive opt-in data
#'
#' @param ... Form data
#' @param row Form data
#' @param is_test internal
#' 
#' @importFrom jsonlite toJSON fromJSON write_json
#' @importFrom httr POST status_code
#' @importFrom stringr str_glue str_extract
#' @importFrom data.table data.table
#' @importFrom DBI dbAppendTable dbConnect dbDisconnect 
#' @importFrom RPostgres Postgres
#' @importFrom lubridate now
#'
#' @name hca
NULL

#' @describeIn hca external form url
..logRow <- function(row, is_test = TRUE) {
  cn <- DBI::dbConnect(RPostgres::Postgres(), 
                       host = "127.0.0.1", 
                       dbname = "api", 
                       user = "www-data")
  on.exit(DBI::dbDisconnect(cn))
  tbnam <- paste0("optinsessions", '_testing'[is_test])
  res <- DBI::dbAppendTable(cn, tbnam, row)
  res == 1 || return(FALSE)
  return(TRUE)
}



#' @describeIn hca external form url
..postData <- function(..., is_test=FALSE) { 
  if (is_test == TRUE) 
    print(data.table::data.table(...))
  
  r <- httr::POST(
    url = "localhost/ocpu/user/api/library/optin/R/store_session/json",
    encode = "json",
    body = list("data" = data.table::data.table(...))
  )
  # return(r)
  
  if (r$status == "201") {
    tmp <- httr::content(r, "parsed")[[1]]
    sid <- r$headers$`x-ocpu-session`
    loc <- stringr::str_glue("/ocpu/tmp/{sid}/files/")
  } else {
    tmp <- NA_character_
    sid <- NA_character_
    loc <- httr::content(r, as = "text")
  }
  
  rw <- data.table::data.table(
    "status" = r$status,
    "ts_gmt" = lubridate::with_tz(lubridate::now(), "GMT"),
    "session_id" = sid,
    "file_name" = tmp,
    "get_path" = loc
  )
  invisible(..logRow(rw, is_test))
}


#' @describeIn hca external form url
#' @export
hca <- function(...) {
  ..postData(...)
}

#' @describeIn hca internal session url
#' @export
store_session <- function(data) {
  tmp <- tempfile(tmpdir = "", pattern = "form", fileext = ".fst")
  fp <- stringr::str_extract(tmp, "(?<=/).+")
  fst::write_fst(x = data, fp)
  invisible(fp)
}
