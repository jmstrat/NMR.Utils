#' Read a table as an NMR data object
#'
#' @param ... parameters are passed to \code{\link{read.table}}
#' @return An NMR data object containing the data
#' @keywords internal
read.table.nmr <- function(...) {
  df=read.table.jms(...)
  return(as.nmr.data.object(df))
}

#' Make a new NMR data object
#'
#' @param ... parameters are passed to \code{\link{data.frame}}
#' @return An NMR data object
#' @keywords internal
nmr.data.object <- function(...) {
  return(as.nmr.data.object(jms.data.object(...)))
}

#' Convert a data.frame to an NMR data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @keywords internal
as.nmr.data.object <- function(x) {
  x=as.jms.data.object(x)
  class(x) <- c('nmr.data.object',class(x))
  x
}

#' Convert an NMR data object to a 2D NMR data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @keywords internal
make.nmr.2Ddata.object <- function(x) {
  x=as.jms.data.object(x)
  class(x) <- c("nmr.2Ddata.object","nmr.data.object",class(x))
  x
}

#' Check if an object is an NMR data object
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @keywords internal
is.nmr.data.object <- function(x) {
  return(inherits(x,"nmr.data.object"))
}

#' Check if an object is a 2D NMR data object
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @keywords internal
is.nmr.2Ddata.object <- function(x) {
  return(inherits(x,"nmr.2Ddata.object"))
}
