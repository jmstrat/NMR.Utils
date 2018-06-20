#' Checks whether any values in a dataframe are complex
#' @param data The dataframe
#' @export
any_complex <- function(data) {
  any(sapply(data, is.complex))
}
