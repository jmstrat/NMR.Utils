#' Convert a 2D NMR data object to an insitu NMR data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @keywords internal
as.nmr2dinsitu.data.object <- function(x) {
  if(is.nmr2d.data.object(x)) {
    x=as.nmr2dinsitu.data.object.super(x)
    attr(x, "echem") <- Echem.Data::echem.data.object()
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
  #Check validity of echem
  if(!Echem.Data::is.echem.data.object(echem)) stop('Echem object must be of class echem.data.object')
  nmr=as.nmr2dinsitu.data.object(nmr)
  attr(nmr, "echem")<- echem
  return(nmr)
}
