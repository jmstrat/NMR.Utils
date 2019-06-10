#' Read 2D NMR Data (real only)
#'
#' This function is used to import NMR data.
#' @param nmrfile Path to txt file saved using topspin's File->Save->Save visible region as text file (topspin doesn't support immaginary)
#' @param imaginary_file Path to txt file as above, but set to 90 degrees out of phase in topspin
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' read.nmr.text.2d.1col("/path/to/file.txt")
#' @keywords internal
read.nmr.text.2d.1col <- function(nmrfile,imaginary_file=NA) {
  jms.classes::log.info('Reading 2D 1 column file: %s', nmrfile)
  #Open file for reading
  con=file(nmrfile,open='r')
  #Read first 8 lines
  lines=readLines(con,n=8)
  #Close file
  close(con)
  #Extract lines
  f2=lines[[5]]
  nr=lines[[7]]
  nc=lines[[8]]

  #Get data from lines
  f2l=as.numeric(gsub("# F2LEFT = ([[:digit:]\\.-]*) .*","\\1",f2))
  f2r=as.numeric(gsub("# F2LEFT = [[:digit:]\\.-]* ppm\\. F2RIGHT = ([[:digit:]\\.-]*) .*","\\1",f2))
  nrows=as.numeric(gsub("# NROWS = ([[:digit:]]*).*","\\1",nr))
  ncols=as.numeric(gsub("# NCOLS = ([[:digit:]]*).*","\\1",nc))

  #Calculate ppm scale
  ppmscale=seq(f2l,f2r,(f2r-f2l)/(ncols-1))

  #Read all data ignoring comment lines (Thus also ignoring row separators)
  fullTable=read.table(nmrfile)
  if(!is.na(imaginary_file)) {
    jms.classes::log.info('Reading 2D 1 column file: %s', imaginary_file)
    imaginaryTable=read.table(imaginary_file)
  }

  #Split table into multiple columns of length ncols
  cols=c()
  for(i in 1:nrows) {
    if(!is.na(imaginary_file)) {
      cols[[i]]=complex(real=fullTable[((i-1)*ncols+1):(i*ncols),],imaginary=imaginaryTable[((i-1)*ncols+1):(i*ncols),])
    } else {
      cols[[i]]=fullTable[((i-1)*ncols+1):(i*ncols),]
    }
  }

  #Make data frame
  nmrdata=nmr.data.object(ppmscale,cols)
  #Rename columns
  names(nmrdata) <- c("ppm",0:(nrows-1))
  #return data
  return(as.nmr2d.data.object(nmrdata))
}
