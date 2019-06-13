#' Generates time offsets if spectra were acquired without ATM
#'
#' @param spectra A dataframe of NMR data
#' @param scan_length Length of a single scan (hours)
#' @param initial_offset Offset of 1st scan (hours)
#' @return A vector of offsets
#' @export
#' @examples
#' noATMoffsets(spectra, 0.5)
noATMoffsets <- function(spectra, scan_length, initial_offset=0) {
  n <- ncol(spectra) - 1
  return(0:n * scan_length + initial_offset)
}

#' Stores the time offsets in the data frame
#'
#' @param spectra A dataframe of NMR data
#' @param offsets A vector of offsets
#' @return An idential data frame whose names correspond to the offsets
#' @export
#' @examples
#' storeOffsets(data, offsets)
storeOffsets <- function(spectra, offsets) {
  jms.classes::log.info("Adding scan offsets to 2D NMR data")
  lenO <- length(offsets)
  lenS <- ncol(spectra) - 1
  if (lenO < lenS) {
    offsets <- append(offsets, (lenO + 1):lenS)
  } else if (lenS < lenO) {
    offsets <- offsets[c(1:lenS)]
  }
  names(spectra)[c(2:(lenS + 1))] <- offsets
  return(spectra)
}
