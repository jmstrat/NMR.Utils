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
