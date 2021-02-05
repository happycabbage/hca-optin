#' Receive opt-in data
#'
#' @param ... Form data
#'
#' @importFrom jsonlite toJSON
#'
#' @export
hca_optin <- function(...) {
  return( jsonlite::toJSON(list(...)) )
}
