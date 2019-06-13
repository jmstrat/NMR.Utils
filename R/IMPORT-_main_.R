#' Read NMR Data
#'
#' This function is used to import 1D or 2D NMR data.
#' Data may be imported either by passing the experiment directory, or a file exported from topspin.
#'
#' If a folder is passed as the parameter to nmrfile, read.NMR will attempt to guess whether it is a folder containing individual scans from an in-situ experiement, a folder containing raw data or a folder containing 1D spectra to be summed. It will then process accordingly.
#' @param nmrfile Path to txt file saved using topspin's File->Save as->Save data of currently displayed region in a text file. Or a directory of files (see above)
#' @param nucleus The nucleus to which the data corresponds (automatically determined if an acqus file is available)
#' @param acqus The path to the directory containing the acqus file for the data (automatically found if a bruker directory is provided)
#' @param ... Further parameters passed to the underlying funcition calls (controlling whether immaginary data is imported, ...)
#' @return A data frame containing the NMR data (ppm,intensity1, ...)
#' @export
#' @examples
#' read.nmr("/path/to/file.txt")
#' read.nmr("/path/to/nmr/expName/expNo")
#' read.nmr("/path/to/folder/")
read.nmr <- function(nmrfile, mass=1, nucleus="Unknown Nucleus", acqus=NA, ...) {
  jms.classes::log.debug("Reading NMR data from %s", nmrfile)
  acqs <- list()

  data <- read.nmr.guess(nmrfile, ...)

  n <- attr(data, "nuc1")
  if (!is.null(n)) nucleus <- n

  if (!length(acqs) && !is.na(acqus)) {
    acqs <- read.acqu(acqus)
    nucleus <- acqs$nuc1
  } else {
    acqs$nuc1 <- nucleus
    ns <- attr(data, "ns")
    if (is.null(ns)) ns <- 1
    acqs$ns <- ns
  }
  if (is.na(mass)) mass <- 1
  acqs$mass <- mass

  for (att in names(acqs)) {
    attr(data, att) <- acqs[[att]]
  }

  if (nucleus == "Unknown Nucleus") {
    axis_label <- expression("Chemical Shift / ppm")
  } else {
    element <- sub("[0-9]*([A-Za-z]*)", "\\1", nucleus)
    mass_number <- sub("([0-9]*)[A-Za-z]*", "\\1", nucleus)
    axis_label <- bquote(delta ~ "("^.(mass_number) * .(element) * ")" ~ "/" ~ ppm)
  }
  jms.classes::xlab(data) <- axis_label

  attr(data, "filepath") <- nmrfile
  attr(data, "filename") <- basename(nmrfile)

  return(data)
}

#' @export
#' @rdname read.nmr
read.NMR <- read.nmr
