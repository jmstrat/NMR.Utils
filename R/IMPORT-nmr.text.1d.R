#' Read 1D NMR Data (1 col)
#'
#' This function is used to import NMR data.
#' @param nmrfile Path to txt file saved using topspin's File->Save->Save visible region as text file (with or without imaginary)
#' @return A data frame containing the NMR data (ppm,intensity)
#' @examples
#' read.nmr.text.1d.1col("/path/to/file.txt")
#' @keywords internal
read.nmr.text.1d.1col <- function(nmrfile) {
  jms.classes::log.info('Reading 1D 1 column file: %s', nmrfile)
  ppmscale=read.nmr.text.1d.1col.ppm(nmrfile)

  #Read all data ignoring comment lines (Thus also ignoring row separators)
  fullTable=read.table(nmrfile)

  #Make data frame
  nmrdata=nmr.data.object(ppmscale,fullTable$V1)
  #Rename columns
  names(nmrdata) <- c("ppm","intensity")
  return(nmrdata)
}

#' Read 1D NMR Data (2 / 4 col)
#'
#' This function is used to import NMR data.
#' @param nmrfile Path to txt file
#' @return A data frame containing the NMR data (ppm,intensity,...)
#' @examples
#' read.nmr.text.1d.2col("/path/to/file.txt")
#' @keywords internal
read.nmr.text.1d.2col <- function(file, skip, sep = "") {
  jms.classes::log.info('Reading 1D 2 column file: %s', file)
  tab = read.table.nmr(file,skip=skip, sep = sep)
  n = ncol(tab)
  if(n == 2) {
    names(tab) <- c("ppm","intensity")
  } else {
    # ascii-spec
    names(tab) <- c("point","intensity","hz","ppm")
  }
  tab
}

#' Read 1D NMR Data
#'
#' This function is used to import NMR data.
#' @param nmrfile Path to txt file saved using topspin's File->Save->Save visible region as text file (with or without imaginary)
#' @return A data frame containing the NMR data (ppm,intensity)
#' @examples
#' read.nmr.text.1d("/path/to/file.txt")
#' @keywords internal
read.nmr.text.1d <- function(file) {
  type_skip=determine_1d_file_type(file)
  if(type_skip[[1]]==1) {
    data = read.nmr.text.1d.1col(file)
  } else if(type_skip[[1]]==2) {
    data = read.nmr.text.1d.2col(file,type_skip[[2]],type_skip[[3]])
  } else {
    stop("File type not supported")
  }
  jms.classes::xcol(data) <- 'ppm'
  jms.classes::ycol(data) <- 'intensity'
  data
}

#' Read ppm scale for 1D data
#'
#' This function attempts to determine the ppm scale from a bruker text file by examining the preamble.
#' @param file Path to txt file saved using topspin's File->Save->Save visible region as text file (with or without imaginary)
#' @return A vector containing the ppm scale
#' @examples
#' read.nmr.text.1d.1col.ppm("/path/to/file.txt")
#' @keywords internal
read.nmr.text.1d.1col.ppm <-function(file) {
  #Extract info from preamble
  #Open file for reading
  con=file(file,open='r')
  #Read first 8 lines
  lines=readLines(con,n=8)
  #Close file
  close(con)
  #Extract lines
  lr=lines[[4]]
  nr=lines[[6]]

  #Get data from lines
  l=as.numeric(gsub("# LEFT = ([[:digit:]\\.-]*) .*","\\1",lr))
  r=as.numeric(gsub("# LEFT = [[:digit:]\\.-]* ppm\\. RIGHT = ([[:digit:]\\.-]*) .*","\\1",lr))
  nrows=as.numeric(gsub("# SIZE = ([[:digit:]]*).*","\\1",nr))

  #Calculate ppm scale
  ppmscale=seq(l,r,(r-l)/(nrows-1))
  return(ppmscale)
}
