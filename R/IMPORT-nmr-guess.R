NMR_TEXT_FILE_PATTERN = "*.(txt|dat)"

read.nmr.guess <- function(path, ...) {
  if(dir.exists(path)) {
    return(read.nmr.guess.directory(path, ...))
  } else if(file.exists(path)) {
    #We're working with a text file
    return(read.nmr.guess.file(path, ...))
  } else {
    stop(path, ' was not found.')
  }
}

read.nmr.guess.directory <- function(path, ...) {
  jms.classes::log.debug('Treating %s as a directory', path)
  #File is a directory, determine whether it is a bruker folder or a normal folder

  # First look for numbered subfolders
  numbered_contents = dir(path, pattern='^[[:digit:]]+$',full.names=T)
  # Then check if any of them are directories
  if(any(dir.exists(numbered_contents))) {
    # We have a brucker experiment folder
    return(read.nmr.directory.bruker.experiment(path, ...))
  }

  #Next check for a pdata folder
  if(dir.exists(paste0(path,"/pdata"))) {
    # We have a bruker process directory
    return(read.nmr.directory.bruker.process(path, ...))
  }
  jms.classes::log.debug('Treating %s as a non-bruker directory', path)
  #We now assume we're working with a folder of text files
  #Two options:
  #1) In-situ files saved as individual scans (e.g. if carrier frequency is changed during scan)
  #2) 1D spectrum with multiple carrier frequencies

  #In order to determine which is which we have to inspect the ppm scale:
  #We assume case 1 if the ppm scale is identical for each scan
  #Otherwise we choose case 2

  #Firstly we find all of the files
  files = jms.classes::list.files.sorted(path,pattern=NMR_TEXT_FILE_PATTERN,full.names=TRUE)
  jms.classes::log.debug('%s contains %s files', path, length(files))
  #We check that we have some files
  if(length(files)==0) {
    stop("No files were found.")
  }

  #Then we inspect the first file
  type_skip=determine_1d_file_type(files[[1]])

  if(type_skip[[1]]==1) {
    return(read.nmr.directory.text.1d.1col(files))
  } else if(type_skip[[1]]==2) {
    return(read.nmr.directory.text.1d.2col(files,type_skip[[2]]),type_skip[[3]])
  } else {
    stop("File type not supported")
  }
}

read.nmr.guess.file <- function(path, imaginary_file=NA, ...) {
  jms.classes::log.debug('Treating %s as a file', path)
  dim=determine_dimensions_from_text_file(path)
  if(dim==1) {
    return(read.nmr.text.1d(path))
  } else if(dim==2) {
    return(read.nmr.text.2d.1col(path,imaginary_file=imaginary_file))
  } else {
    stop("Input file not supported.")
  }
}

#' Determine NMR file type
#'
#' This function attempts to determine the type of a bruker text file
#' @param file File to read
#' @return An integer representing the file type
#' @examples
#' determine_1d_file_type(file)
#' @keywords internal
determine_1d_file_type <- function(file) {
  tryCatch({
    #Extract info from preamble
    #Open file for reading
    con=file(file,open='r')
    #Read first line
    lines=readLines(con,n=2)
    #Close file
    close(con)
    #Extract lines
    l1=lines[[1]]

    if(grepl("^# File created", l1)) {
      return(c(1,NA,NA))
    } else if(grepl("^ppm scale", l1)) {
      return(c(2,3,""))
    } else if(grepl("^[-+eE.0-9]* [-+eE.0-9]*$", l1)) {
      #2 columns of data, no header
      return(c(2,0,""))
    } else {
      n <- tryCatch({
        ncol(read.table(text=lines[[2]]))
      }, error = function(e) {
        0
      })
      if( n == 4) {
        # ascii-spec
        return(c(2,1,","))
      }

      stop("Input file not supported.")
    }
  }, error = function(e) {
    jms.classes::log.error(e)
    stop("Input file not supported.")
  })
}

#' Read NMR Data Dimensions
#'
#' This function attempts to determine the dimension of a bruker text file by examining the preamble.
#' @param file Path to txt file saved using topspin's File->Save->Save visible region as text file (with or without imaginary)
#' @return A number indicating the dimensions
#' @examples
#' determine_dimensions_from_text_file("/path/to/file.txt")
#' @keywords internal
determine_dimensions_from_text_file <-function(file) {
  tryCatch({
    #Extract info from preamble
    #Open file for reading
    con=file(file,open='r')
    #Read first 5 lines
    lines=readLines(con, n=5)
    #Close file
    close(con)
    #Extract lines
    l4=lines[[4]]
    l5=lines[[5]]

    if(grepl("^# LEFT", l4)) {
      return(1)
    } else if(grepl("^# F2LEFT", l5)) {
      return(2)
    } else {
      n <- tryCatch({
        ncol(read.table(text=lines[[2]]))
      }, error = function(e) {
        0
      })
      if( n == 4) {
        # ascii-spec
        return(1)
      }
      stop("Input file not supported.")
    }
  }, error = function(e) {
    jms.classes::log.error(e)
    stop("Input file not supported.")
  })
}
