#' Convert a 2D NMR data object to an insitu NMR data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @keywords internal
make.insitu.nmr.2Ddata.object <- function(x) {
  if(is.nmr.2Ddata.object(x)) {
    if(load_or_install('Echem',optional=TRUE)) {
      attr(x, "class") <- c("insitu.nmr.2Ddata.object",class(x))
      attr(x, "echem") <- dummy.echem()
    } else {
      stop('In situ NMR processing requires the Echem library')
    }
  } else {
    stop("make.insitu.nmr.2Ddata.object can only be called with an nmr.2Ddata.object")
  }
  return(x)
}

#' Check if an object is an insitu NMR data object
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @keywords internal
is.insitu.nmr.2Ddata.object <- function(x) {
  return(inherits(x,"insitu.nmr.2Ddata.object"))
}

#' Make an insitu NMR data object containing both NMR and Echem data
#'
#' @param nmr An nmr.2Ddata.object
#' @param Echem The echem data
#' @return An insitu.nmr.2Ddata.object
#' @export
associate_echem_with_nmr <- function(nmr,echem) {
  if(!load_or_install('Echem',optional=TRUE)) stop('In situ NMR processing requires the Echem library')
  #Check validity of echem
  nmr=make.insitu.nmr.2Ddata.object(nmr)
  if(!is.echem.data.object(echem)) stop('Echem object must be of class echem.data.object')
  attr(nmr, "echem")<- echem
  return(nmr)
}
