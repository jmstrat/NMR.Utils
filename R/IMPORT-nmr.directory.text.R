#' Process a folder of two column text files
#'
#' This function attempts process 1D data files in a directory
#' @param files List of file paths to read
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' read.nmr.directory.1d.2col(files)
#' @keywords internal
read.nmr.directory.text.1d.2col <- function(files, skip, sep) {
  jms.classes::log.info("Processing directory containing 1D 2 column files")
  .read.nmr.directory.text(files, read.nmr.text.1d.2col, skip=skip, sep=sep)
}

#' Process a folder of one column text files
#'
#' This function attempts process 1D data files in a directory
#' @param files List of file paths to read
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' read.nmr.directory.1d.2col(files)
#' @keywords internal
read.nmr.directory.text.1d.1col <- function(files) {
  jms.classes::log.info("Processing directory containing 1D 1 column files")
  .read.nmr.directory.text(files, read.nmr.text.1d.1col)
}

.read.nmr.directory.text <- function(files, func, ...) {
  # Extract ppm scales for the first two files
  ppm1 <- func(files[[1]], ...)$ppm
  ppm2 <- func(files[[2]], ...)$ppm

  # Are they the same?
  if (all(ppm1 == ppm2)) {
    # Assume in situ
    # Now we have to read each file as a subsequent column for a data frame
    columns <- c()
    for (f in 1:length(files)) {
      data <- func(files[[f]], ...)
      if (!all(data$ppm == ppm1)) {
        stop("Unable to determine whether 2d or 1d experiment")
      }
      columns[[f]] <- data$intensity
    }
    nmrdata <- nmr2d.data.object(ppm1, columns)
    # Rename columns
    names(nmrdata) <- c("ppm", 0:(ncol(nmrdata) - 2))
    # return data
    return(nmrdata)
  } else {
    # TODO Assume VOCS
    stop("Handling a directory of 1D spectra with different ppm axes has not yet been implemented.")
  }
}
