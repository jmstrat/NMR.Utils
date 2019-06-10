#' Make an insitu NMR data object containing both NMR and Echem data
#'
#' @param nmr An nmr.2Ddata.object
#' @param Echem The echem data
#' @return An insitu.nmr.2Ddata.object
#' @export
associate_echem_with_nmr <- function(nmr,echem) {
  if(!requireNamespace("Echem.Data", quietly=TRUE)) {
    stop('Insitu data processing requires the Echem.Data package')
  }
  #Check validity of echem
  if(!Echem.Data::is.echem.data.object(echem)) stop('Echem object must be of class echem.data.object')
  if(length(xcol(echem)) != 1 || is.na(xcol(echem))) {
    jms.classes::log.error(
      'xcol is not correct: xcol = [%s]; ycol = [%s]',
      paste0(xcol(echem), collapse=','),
      paste0(ycol(echem), collapse=','))
    stop('Invalid echem data')
  }

  if(length(ycol(echem)) != 1 || is.na(ycol(echem))) {
    jms.classes::log.error(
      'ycol is not correct: xcol = [%s]; ycol = [%s]',
      paste0(xcol(echem), collapse=','),
      paste0(ycol(echem), collapse=','))
    stop('Invalid echem data')
  }

  jms.classes::log.info('Combining echem and NMR objects')
  nmr=as.nmr2dinsitu.data.object(nmr)
  attr(nmr, "echem")<- echem
  return(nmr)
}
