#' Export NMR Data
#'
#' This function is used to export 1D or 2D NMR data as a csv file.
#' @param data The data object
#' @param filename The path to the file (ending in .csv)
#' @param scan_number For 2D data, the scan number may be (optionally) specified
#' @return None
#' @export
#' @examples
#' export_nmr(data,"/path/to/file.csv")
#'
#' export_nmr(data,"/path/to/file.csv",scan_number=1)
export_nmr <- function(data,filename,scan_number=NA) {
	if(is.na(scan_number) | !scan_number <=ncol(data)-1) {
		write.table(data,filename,sep=",",row.names =FALSE)
	} else {
		write.table(data[,c(1,scan_number+1)],filename,sep=",",row.names =FALSE)
	}
}

#' Export echem data
#'
#' This function is used to export echem data as a csv file.
#' @param echem The echem data object
#' @param filename The path to the file (ending in .csv)
#' @return None
#' @export
#' @examples
#' export_echem(echem,"/path/to/file.csv")
export_echem <- function(echem,filename) {
	write.table(echem,filename,sep=",",row.names =FALSE)
}

#' Export fit results
#'
#' This function is used to export the fit results as a csv file.
#' @param fit The fit object
#' @param filename The path to the file (ending in .csv)
#' @return None
#' @export
#' @examples
#' export_fit(fit,"/path/to/file.csv")
export_fit <- function(fit,filename) {
	write.table(fit$result,filename,sep=",",row.names =FALSE)
}

#' Save fit
#'
#' This function is used to save the fit as an R binary file. The resulting file can be read using (\code{\link[NMR.Utils]{load_fit}}).
#' @param fit The fit object
#' @param filename The path to the file (ending in .R)
#' @return None
#' @export
#' @examples
#' save_fit(fit,"/path/to/file.R")
save_fit <-function(fit,filename) {
  save(fit,file=filename)
}

#' Load fit
#'
#' This function is used to load a fit saved using (\code{\link[NMR.Utils]{save_fit}}).
#' @param filename The path to the file (ending in .R)
#' @return None
#' @export
#' @examples
#' fit = load_fit("/path/to/file.R")
load_fit <-function(filename) {
  return(load(filename))
}