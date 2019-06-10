#' Rebins the data to ensure a minimum offset between scans
#'
#' @param data 2D NMR data
#' @param min_offset Minimum offset between adjacent scans. Scans too close together will be dealt
#'                   with according to the \code{method} option.
#' @param method One of the following:
#' \describe{
#'  \item{discard}{Scans that are not sufficiently offset from the previous scan are discarded.}
#'  \item{average_before}{Scans that are not sufficiently offset from the previous scan are averaged with the previous scan.}
#'  \item{average_after}{Scans that are not sufficiently offset from the previous scan are averaged with the next scan.}
#' }
#' @export
#' @examples
#' rebin(data, 10)
#' rebin(data, 10, 'average_before')
rebin <- function(data, min_offset, method=c('discard', 'average_before', 'average_after')) {
  method = method[[1]]
  jms.classes::log.info('Rebinning data (method=%s)', method)

  offsets = as.numeric(colnames(data)[-1])

  scans_to_remove = c()
  scans_to_average = c()

  poff=-Inf
  last_good=NA
  for(i in 1:length(offsets)) {
    off=offsets[[i]]
    if (abs(off - poff) < min_offset) {
      scans_to_remove = append(scans_to_remove, i)

      if (method == 'average_before') {
        data[,last_good + 1] <- rowMeans(data[,c(last_good, i) + 1])
      } else if (method == 'average_after') {
        scans_to_average = append(scans_to_average, i)
      }

    } else {
      poff = off
      last_good = i

      if (length(scans_to_average) > 0) {
        scans_to_average = append(scans_to_average, i)
        data[,i+1] <- rowMeans(data[, scans_to_average + 1])
      }
      scans_to_average = c()
    }
  }
  if (length(scans_to_remove) > 0) {
    jms.classes::log.debug('Rebinning removed %s scans', length(scans_to_remove))
    return(data[, -(scans_to_remove + 1)])
  }
  jms.classes::log.debug('Rebinning did not remove any scans')
  data
}
