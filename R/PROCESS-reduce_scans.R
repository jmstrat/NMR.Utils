#' Reduces the number of scans in the data frame
#'
#' @param spectra A dataframe of NMR data
#' @param scan_range A vector of scans to include c(first, last)
#' @return An idential data frame whose scans are limited to those in scan_range
#' @export
#' @examples
#' reduceScans(spectra,c(1,50))
reduceScans <-function(spectra,scan_range) {
  if(missing(scan_range)) scan_range <- c(1, find_last_scan(spectra))
  scan_range=scan_range+1
  if(scan_range[[1]]<2)
    scan_range[[1]]=2
  n=ncol(spectra)
  if(scan_range[[2]]>n)
    scan_range[[2]]=n
  spectra[,c(1,scan_range[[1]]:scan_range[[2]])]
}

#' Finds the number of the last (non-empty) scan
#'
#' @param spectra A dataframe of NMR data
#' @param tol If all the intensities are equal to within this tolerance the scan is considered empty
#' @return The number of the last scan
#' @export
#' @examples
#' find_last_scan(spectra)
find_last_scan <- function(spectra, tol=0.01) {
  not_missing_scans <- which(!sapply(spectra, function(y) abs(max(y) - min(y)) < tol))
  if(length(not_missing_scans)) return(not_missing_scans[[length(not_missing_scans)]]-1)
  0
}
