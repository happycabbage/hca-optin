#' Receive opt-in data
#'
#' @param ... Form data
#'
#' @importFrom jsonlite toJSON
#'
#' @export
hca <- function(...) {
  return( jsonlite::toJSON(list(...)) )
}
