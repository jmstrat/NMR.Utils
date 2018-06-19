
#' Reduces the number of scans in the data frame
#'
#' @param spectra A dataframe of NMR data
#' @param scan_range A vector of scans to include c(first, last)
#' @return An idential data frame whose scans are limited to those in scan_range
#' @export
#' @examples
#' reduceScans(spectra,c(1,50))
reduceScans <-function(spectra,scan_range) {
  scan_range=scan_range+1
  if(scan_range[[1]]<2)
    scan_range[[1]]=2
  n=ncol(spectra)
  if(scan_range[[2]]>n)
    scan_range[[2]]=n
  spectra[,c(1,scan_range[[1]]:scan_range[[2]])]
}
