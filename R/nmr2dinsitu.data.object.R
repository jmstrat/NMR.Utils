#' Convert a 2D NMR data object to an insitu NMR data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @keywords internal
as.nmr2dinsitu.data.object <- function(x) {
  if(is.nmr2d.data.object(x)) {
    if(load_or_install('Echem',optional=TRUE)) {
      x=as.nmr2dinsitu.data.object.super(x)
      attr(x, "echem") <- dummy.echem()
    } else {
      stop('In situ NMR processing requires the Echem library')
    }
  } else {
    stop("as.nmr2dinsitu.data.object can only be called with an nmr2d.data.object")
  }
  return(x)
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
  nmr=as.nmr2dinsitu.data.object(nmr)
  if(!is.echem.data.object(echem)) stop('Echem object must be of class echem.data.object')
  attr(nmr, "echem")<- echem
  return(nmr)
}
