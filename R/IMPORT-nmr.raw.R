#' Read 2D NMR Data (processed, complex)
#'
#' This function is used to import NMR data.
#' @param dir Path to raw data directory (up to expNo)
#' @param acqs List of acquisition parameters (\code{\link[NMR.Utils]{read.acqu}})
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' read.NMR2D.raw("/path/to/nmr/expName/expNo")
#' @keywords internal
read.NMR2D.raw <- function(dir,acqs) {

  Warning("Reading the raw format currently doesn't work as I don't understand the submatrix format. See ? read.nmr.text.2d.1col for a workaround.")
  #reading offset from proc files if possible:
  if(!file.exists(paste0(dir,"/pdata/1/procs"))) {
    stop("Unable to find processed data")
  }
  con=file(paste0(dir,"/pdata/1/procs"),open='r')
  head0=readLines(con)
  close(con)
  os1=as.numeric(gsub('##\\$OFFSET=',"",head0[grepl('##\\$OFFSET=',head0)]))
  nPoints=as.numeric(gsub('##NPOINTS= ([[:digit:]]*).*',"\\1",head0[grepl('##\\NPOINTS=',head0)]))

  ppm = os1 - acqs$sw[[1]] + acqs$sw[[1]]*c(1:(acqs$td[[1]]/2))/(acqs$td[[1]]/2)

  rfile=paste0(dir,"/pdata/1/2rr")
  if(!file.exists(rfile)) {
    stop("Unable to find fourier transformed data")
  }
  ifile=paste0(dir,"/pdata/1/2ii")
  if(!file.exists(ifile)) {
    stop("Unable to find fourier transformed data")
  }

  rawdatF=file(rfile,open='rb')
  Rdata=readBin(rawdatF, 'int',n=file.info(rfile)$size/4, size=4,endian = "little") #integers are 4 bytes (32 bits)
  close(rawdatF)
  rawdatF=file(ifile,open='rb')
  Idata=readBin(rawdatF, 'int',n=file.info(ifile)$size/4, size=4,endian = "little") #integers are 4 bytes (32 bits)
  close(rawdatF)
  Cdata=complex(real=Rdata,imaginary=Idata)

  fsize=length(Cdata)
  Cdata=matrix(Cdata, nrow = length(Cdata)/nPoints, ncol = nPoints,byrow=F)
  nmrdata=nmr.data.object(ppm,Cdata)
  names(nmrdata) <- c("ppm",0:(nPoints-1))
  return (as.nmr2d.data.object(nmrdata))
}


#TODO: see fid_read.R
#' #' Read 1D NMR Data (processed, complex)
#' #'
#' #' This function is used to import NMR data.
#' #' @param dir Path to raw data directory (up to expNo)
#' #' @param acqs List of acquisition parameters (\code{\link[NMR.Utils]{read.acqu}})
#' #' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' #' @examples
#' #' read.NMR1D.raw("/path/to/nmr/expName/expNo")
#' #' @keywords internal
#' read.NMR1D.raw <- function(dir,acqs) {
#'
#' }


