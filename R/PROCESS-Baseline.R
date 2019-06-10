#' NMR Baseline correction
#'
#' Corrects the baseline for NMR Spectra
#' @param data A data frame of the form [ppm, intensity1, ...]
#' @param ... Additional parameters are passed to the baseline function (\code{\link[baseline]{baseline}})
#' @return An identical data frame containing the baseline corrected data
#' @export
#' @examples
#' correct_baseline(data, method="modpolyfit",degree=1)
correct_baseline <-function(data,...) {
  #Perform baseline correction
  base <- baseline::baseline(t(data[,2:ncol(data)]),...)
  data[,2:ncol(data)]=t(baseline::getCorrected(base))
  data
}

#' NMR Baseline Analysis
#'
#' Analyses the baseline for NMR Spectra graphically. See \code{\link[baseline]{baselineGUI}} for information on additional software dependencies.
#' @param data A data frame of the form [ppm, intensity1, ...]
#' @return None
#' @export
#' @examples
#' analyse_baseline(data)
analyse_baseline <-function(data) {
	baseline::baselineGUI(t(data[,2:length(data)]),rev.x=T,labels=data[,1])
}
