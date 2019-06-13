#' Make an insitu NMR data object containing both NMR and Echem data
#'
#' @param nmr An nmr.2Ddata.object
#' @param Echem The echem data
#' @return An insitu.nmr.2Ddata.object
#' @export
associate_echem_with_nmr <- function(nmr, echem) {
  if (!requireNamespace("Echem.Data", quietly=TRUE)) {
    stop("Insitu data processing requires the Echem.Data package")
  }
  # Check validity of echem
  if (!Echem.Data::is.echem.data.object(echem)) stop("Echem object must be of class echem.data.object")
  if (length(xcol(echem)) != 1 || is.na(xcol(echem))) {
    jms.classes::log.error(
      "xcol is not correct: xcol = [%s]; ycol = [%s]",
      paste0(xcol(echem), collapse=","),
      paste0(ycol(echem), collapse=",")
    )
    stop("Invalid echem data")
  }

  if (length(ycol(echem)) != 1 || is.na(ycol(echem))) {
    jms.classes::log.error(
      "ycol is not correct: xcol = [%s]; ycol = [%s]",
      paste0(xcol(echem), collapse=","),
      paste0(ycol(echem), collapse=",")
    )
    stop("Invalid echem data")
  }

  jms.classes::log.info("Combining echem and NMR objects")
  nmr <- as.nmr2dinsitu.data.object(nmr)
  attr(nmr, "echem") <- echem
  if (is.null(attr(nmr, ".scantimes"))) attr(nmr, ".scantimes") <- as.numeric(colnames(nmr)[-1])
  transformOffsetsToXAxis(nmr, echem)
}

transformOffsetsToXAxis <- function(nmr, echem) {
  # Do we need to transform?
  xc <- jms.classes::xcol(echem)
  currencXC <- attr(nmr, ".offset_xcol")
  if (is.null(currencXC)) currencXC <- which(colnames(echem) == "Test_Time.s.")
  if (currencXC == xc) {
    return(nmr)
  }
  attr(nmr, ".offset_xcol") <- xc
  jms.classes::log.info("Adjusting offsets to match %s", colnames(echem)[xc])
  # Transform the offsets
  offsets <- attr(nmr, ".scantimes")
  new_offsets <- Plotting.Utils::nearest(echem, Test_Time.s.=offsets * 3600)[, xc] * jms.classes::xscale(echem)
  storeOffsets(nmr, new_offsets)
}
