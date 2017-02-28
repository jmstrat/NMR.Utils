#' Read Acquisition Parameters
#'
#' This function is read acquisition pararameters for NMR Data.
#' @param dir Path to raw data directory (up to expNo)
#' @return A list of parameters
#' @export
#' @examples
#' read.acqu("/path/to/nmr/expName/expNo")
read.acqu <- function(dir) {
  if(file.exists(paste0(dir,"/acqus"))) {
    con=file(paste0(dir,"/acqus"),open='r')
    head0=readLines(con)
    close(con)
    position = grep('^\\$\\$',head0)[[1]]
    acqu=list()
    acqu$file       = gsub("^\\$\\$ ","",head0[[position+1]])

    get_pat <- function(pat) {
      gsub(pat,"",head0[grepl(pat,head0)])
    }

    acqu$aq_mod     = suppressWarnings(as.numeric(get_pat("^##\\$AQ_mod= ")))
    acqu$bytorda    = suppressWarnings(as.numeric(get_pat('##\\$BYTORDA= ')))
    acqu$cnst       = suppressWarnings(as.numeric(get_pat('##\\$CNST= ')))
    acqu$d          = suppressWarnings(as.numeric(get_pat('##\\$D= ')))
    acqu$de         = suppressWarnings(as.numeric(get_pat('##\\$DE= ')))
    acqu$decim      = suppressWarnings(as.numeric(get_pat('##\\$DECIM= ')))
    acqu$digmod     = suppressWarnings(as.numeric(get_pat('##\\$DIGMOD= ')))
    acqu$ds         = suppressWarnings(as.numeric(get_pat('##\\$DS= ')))
    acqu$dspfvs     = suppressWarnings(as.numeric(get_pat('##\\$DSPFVS= ')))
    acqu$fw         = suppressWarnings(as.numeric(get_pat('##\\$FW= ')))
    acqu$gpx        = suppressWarnings(as.numeric(get_pat('##\\$GPX= ')))
    acqu$gpy        = suppressWarnings(as.numeric(get_pat('##\\$GPY= ')))
    acqu$gpz        = suppressWarnings(as.numeric(get_pat('##\\$GPZ= ')))
    acqu$in_        = suppressWarnings(as.numeric(get_pat('##\\$IN= ')))  ###extra underscore in r
    acqu$inp        = suppressWarnings(as.numeric(get_pat('##\\$INP= ')))
    acqu$l          = suppressWarnings(as.numeric(get_pat('##\\$L= ')))
    acqu$nbl        = suppressWarnings(as.numeric(get_pat('##\\$NBL= ')))
    acqu$ns         = suppressWarnings(as.numeric(get_pat('##\\$NS= ')))
    acqu$o1         = suppressWarnings(as.numeric(get_pat('##\\$O1= ')))
    acqu$parmode    = suppressWarnings(as.numeric(get_pat('##\\$PARMODE= ')))
    acqu$pl         = suppressWarnings(as.numeric(get_pat('##\\$PL= ')))
    acqu$l          = suppressWarnings(as.numeric(get_pat('##\\$L= ')))
    acqu$pulprog    = get_pat('##\\$PULPROG= ')
    acqu$pulprog    = substr(acqu$pulprog,2,nchar(acqu$pulprog)-1)
    acqu$rg         = suppressWarnings(as.numeric(get_pat('##\\$RG=')))
    acqu$sfo1       = suppressWarnings(as.numeric(get_pat('##\\$SFO1=')))
    acqu$sw         = suppressWarnings(as.numeric(get_pat('##\\$SW=')))
    acqu$sw_h       = suppressWarnings(as.numeric(get_pat('##\\$SW_h=')))
    acqu$td         = suppressWarnings(as.numeric(get_pat('##\\$TD=')))
    acqu$td[[2]]    = 1
    acqu$td[[3]]    = 1
    acqu$td[[4]]    = 1
    acqu$te         = suppressWarnings(as.numeric(get_pat('##$TE=')))
    acqu$vdlist     = get_pat('##\\$VDLIST= ')
    acqu$vdlist     = substr(acqu$vdlist,3,nchar(acqu$vdlist)-1)
    acqu$vplist     = get_pat('##\\$VPLIST= ')
    acqu$vplist     = substr(acqu$vplist,3,nchar(acqu$vplist)-1)
    acqu$vtlist     = get_pat('##\\$VTLIST=')
    acqu$vtlist     = substr(acqu$vtlist,3,nchar(acqu$vtlist)-1)
    acqu$nuc1       = get_pat('##\\$NUC1=')
    acqu$nuc1       = substr(acqu$nuc1,3,nchar(acqu$nuc1)-1)
  } else {
    stop('ERROR: Could not find "acqus"')
  }

  if(acqu$parmode > 0) {
    if(file.exists(paste0(dir,"/acqu2s"))) {
      con=file(paste0(dir,"/acqu2s"),open='r')
      head0=readLines(con)
      close(con)

      acqu$sw[[2]]         = as.numeric(get_pat('##\\$SW='))
      acqu$sw_h[[2]]       = as.numeric(get_pat('##\\$SW_h='))
      acqu$td[[2]]         = as.numeric(get_pat('##\\$TD='))

      if(acqu$parmode > 1) {
        if(file.exists(paste0(dir,"/acqu3s"))) {
          con=file(paste0(dir,"/acqu3s"),open='r')
          head0=readLines(con)
          close(con)
          acqu$sw[[3]]         = as.numeric(get_pat('##\\$SW='))
          acqu$sw_h[[3]]       = as.numeric(get_pat('##\\$SW_h='))
          acqu$td[[3]]         = as.numeric(get_pat('##\\$TD='))

          if(acqu$parmode > 2) {
            if(file.exists(paste0(dir,"/acqu4s"))) {
              con=file(paste0(dir,"/acqu4s"),open='r')
              head0=readLines(con)
              close(con)
              acqu$sw[[4]]         = as.numeric(get_pat('##\\$SW='))
              acqu$sw_h[[4]]       = as.numeric(get_pat('##\\$SW_h='))
              acqu$td[[4]]         = as.numeric(get_pat('##\\$TD='))
            } else {
              stop('ERROR: Could not find "acqu4s"');
            }
          }
        } else {
          stop('ERROR: Could not find "acqu3s"');
        }
      }
    } else {
      stop('ERROR: Could not find "acqu2s"')
    }
  }
  return(acqu)
}

#' Read NMR Data
#'
#' This function is used to import 1D or 2D NMR data.
#' To import 1D data, you must export it from topspin.
#' 2D data may be imported by passing the path to the directory, provided it has been processed using topspin (this allows for imaginary data to be imported) (Doesn't yet work!).
#'
#' If a folder is passed as the parameter to nmrfile, read.NMR will attempt to guess whether it is a folder containing individual scans from an in-situ experiement, a folder containing raw data or a folder containing 1D spectra to be summed. It will then process accordingly.
#' @param nmrfile Path to txt file saved using topspin's File->Save as->Save data of currently displayed region in a text file. Or a directory of files (see above)
#' @param nucleus The nucleus to which the data corresponds
#' @param acqus The path to the directory containing the acqus file for the data
#' @return A data frame containing the NMR data (ppm,intensity1, ...)
#' @export
#' @examples
#' read.NMR("/path/to/file.txt")
#' read.NMR("/path/to/nmr/expName/expNo")
#' read.NMR("/path/to/folder/")
read.NMR <- function(nmrfile,imaginary_file=NA, mass=1,nucleus='Unknown Nucleus', acqus=NA) {
  acqs=list()
  if(dir.exists(nmrfile)) {
    #File is a directory, determine whether it is a bruker folder or a normal folder
    #If there is a pdata folder, we process as raw data, otherwise we assume that it is a folder of text files
    if(dir.exists(paste0(nmrfile,"/pdata"))) {
      #we're working with raw data
      acqs = suppressWarnings(read.acqu(nmrfile))
      dim=acqs$parmode +1

      if(dim==1) {
        stop("Cannot process raw data for 1D experiments, use topspin's export via File->Save as->Save data of currently displayed region in a text file instead. (This also allows for imaginary data).")
      } else if (dim==2) {
        data = (read.NMR2D.raw(nmrfile,acqs))
      } else {
        stop("Only 1D & 2D data are supported.")
      }
    } else {
      #We're working with a folder of text files
      #Two options:
      #1) In-situ files saved as individual scans (e.g. if carrier frequency is changed during scan)
      #2) 1D spectrum with multiple carrier frequencies

      #In order to determine which is which we have to inspect the ppm scale:
      #We assume case 1 if the ppm scale is identical for each scan
      #Otherwise we choose case 2

      #Firstly we find all of the files
      files=list.files(nmrfile,pattern="*.txt",full.names=TRUE)
      files2=list.files(nmrfile,pattern="*.txt",full.names=FALSE)
      as.numeric(gsub('([0123456789]*)\\.txt$','\\1',files2))->fileNum;
      files=files[order(fileNum)]
      #We check that we have some files
      if(length(files)==0) {
        stop("No files were found.")
      }

      #Then we inspect the first file
      type=determine_1d_file_type(files[[1]])

      if(type==1) {
        data = (process_one_column_files(files))
      } else if(type==2) {
        data = (process_two_column_files(files))
      } else {
        stop("File type not supported")
      }
    }
  } else {
    #We're working with a text file

    dim=determine_dimensions_from_text_file(nmrfile)

    if(dim==1) {
      data = (read.NMR1D(nmrfile))
    } else if(dim==2) {
      data = (read.NMR2D.text(nmrfile,imaginary_file=imaginary_file))
    } else {
      stop("Input file not supported.")
    }
  }
  if(!length(acqs) && !is.na(acqus)) {
    acqs=read.acqu(acqus)
  } else {
    acqs$nuc1=nucleus
  }
  acqs$mass=mass
  for(att in names(acqs)) {
    attr(data,att)<-acqs[[att]]
  }
  return(data)
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
    #Read first 8 lines
    lines=readLines(con,n=1)
    #Close file
    close(con)
    #Extract lines
    l1=lines[[1]]

    if(grepl("^# File created", l1)) {
      return(1)
    } else if(grepl("^ppm scale", l1)) {
      return(2)
    } else {
      stop("Input file not supported.")
    }
  }, error = function(e) {
    stop("Input file not supported.")
  })
}

#' Process a folder of two column text files
#'
#' This function attempts process 1D data files in a directory
#' @param files List of file paths to read
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' process_two_column_files(files)
#' @keywords internal
process_two_column_files <- function(files) {
  #Must be 1D
  #At this point we assume that all the text files in the folder are 1D NMR files.

  #Now we extract ppm scales for the first two files
  ppm1=read.table(files[[1]],skip=3)[,1]
  ppm2=read.table(files[[2]],skip=3)[,1]

  #Are they the same?
  if(all(ppm1==ppm2)) {
    #Assume case 1
    return(read_dir_1d_2col_as_2d(files))
  } else {
    #Assume case 2
    #TODO
    warning("Summing 1D spectra has not yet been implemented. (Coming soon!)")
  }
}

#' Process a folder of one column text files
#'
#' This function attempts process 1D data files in a directory
#' @param files List of file paths to read
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' process_one_column_files(files)
#' @keywords internal
process_one_column_files <- function(files) {
  #Get dimensions
  dim=determine_dimensions_from_text_file(files[[1]])

  #If this function returns, it's a valid file.
  #We then make sure it's 1D
  if(!dim==1) {
    #If it's not 1D, we issue an error
    stop("read.NMR can only process directories of 1 dimensional data.")
  }

  #At this point we assume that all the text files in the folder are 1D NMR files.

  #Now we extract ppm scales for the first two files
  ppm1=read_ppm_1D(files[[1]])
  ppm2=read_ppm_1D(files[[2]])

  #Are they the same?
  if(all(ppm1==ppm2)) {
    #Assume case 1
    return(read_dir_1d_as_2d(files))
  } else {
    #Assume case 2
    #TODO
    warning("Summing 1D spectra has not yet been implemented. (Coming soon!)")
  }
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
    #Read first 8 lines
    lines=readLines(con,n=8)
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
      stop("Input file not supported.")
    }
  }, error = function(e) {
    stop("Input file not supported.")
  })
}

#' Read ppm scale for 1D data
#'
#' This function attempts to determine the ppm scale from a bruker text file by examining the preamble.
#' @param file Path to txt file saved using topspin's File->Save->Save visible region as text file (with or without imaginary)
#' @return A vector containing the ppm scale
#' @examples
#' read_ppm_1D("/path/to/file.txt")
#' @keywords internal
read_ppm_1D <-function(file) {
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

#' Generate 2D data from a list of 1D data files
#'
#' This function attempts combine 1D data files into a 2D spectrum
#' @param files List of file paths to read
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' read_dir_1d_as_2d(files)
#' @keywords internal
read_dir_1d_as_2d <-function(files) {
  ppmscale=read_ppm_1D(files[[1]])
  #Now we have to read each file as a subsequent column for a data frame
  columns=c()
  len_f=length(files)
  for(f in 1:len_f) {
    intensities=read.table(files[[f]])
    columns[[f]]=intensities$V1
  }
  nmrdata=data.frame(ppmscale,columns)
  #Rename columns
  names(nmrdata) <- c("ppm",0:(len_f-1))
  #return data
  return(nmrdata)
}

#' Generate 2D data from a list of 1D data files
#'
#' This function attempts combine 1D data files into a 2D spectrum
#' @param files List of file paths to read
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' read_dir_1d_2col_as_2d(files)
#' @keywords internal
read_dir_1d_2col_as_2d <-function(files) {
  ppmscale=read.table(files[[1]],skip=3)[,1]
  #Now we have to read each file as a subsequent column for a data frame
  columns=c()
  len_f=length(files)
  for(f in 1:len_f) {
    columns[[f]]=read.table(files[[f]],skip=3)[,2]
  }
  nmrdata=data.frame(ppmscale,columns)
  #Rename columns
  names(nmrdata) <- c("ppm",0:(ncol(nmrdata)-2))
  #return data
  return(nmrdata)
}

#' Read 1D NMR Data
#'
#' This function is used to import NMR data.
#' @param nmrfile Path to txt file saved using topspin's File->Save->Save visible region as text file (with or without imaginary)
#' @return A data frame containing the NMR data (ppm,intensity)
#' @examples
#' read.NMR1D("/path/to/file.txt")
#' @keywords internal
read.NMR1D <- function(nmrfile) {
  ppmscale=read_ppm_1D(nmrfile)

  #Read all data ignoring comment lines (Thus also ignoring row separators)
  fullTable=read.table(nmrfile)

  #Make data frame
  nmrdata=data.frame(ppmscale,fullTable$V1)
  #Rename columns
  names(nmrdata) <- c("ppm","intensity")
  return(nmrdata)
}

#' Read 2D NMR Data (real only)
#'
#' This function is used to import NMR data.
#' @param nmrfile Path to txt file saved using topspin's File->Save->Save visible region as text file (topspin doesn't support immaginary)
#' @param imaginary_file Path to txt file as above, but set to 90 degrees out of phase in topspin
#' @return A data frame containing the NMR data (ppm,intensity1, intensity2, ...)
#' @examples
#' read.NMR2D.text("/path/to/file.txt")
#' @keywords internal
read.NMR2D.text <- function(nmrfile,imaginary_file=NA) {
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
  nmrdata=data.frame(ppmscale,cols)
  #Rename columns
  names(nmrdata) <- c("ppm",0:(nrows-1))
  #return data
  nmrdata
}

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

    Warning("Reading the raw format currently doesn't work as I don't understand the submatrix format. See ? read.NMR2D.text for a workaround.")
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
  nmrdata=data.frame(ppm,Cdata)
  names(nmrdata) <- c("ppm",0:(nPoints-1))
  return (nmrdata)
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
