read.nmr.directory.bruker.experiment <- function(path, experiment_nos=NA, ...) {
  if(any(is.na(experiment_nos))) {
    numbered_contents = dir(path, pattern='^[[:digit:]]+$',full.names=T)
  } else {
    numbered_contents = paste0(path,'/',experiment_nos)
  }

  dirs = dir.exists(numbered_contents)
  experiments = numbered_contents[dirs]

  if(!length(experiments)) stop('No experiment directories found')

  ppm_scale = c()
  columns=c()
  t0=-1
  # We now need to go through each experiments directory to check for files
  # For now we assume we're making a 2d dataset
  # TODO -- read all experients into list, try to determine most appropriate method (2d, sum, 1dlist etc.)
  # allow override via method argument
  for(i in 1:length(experiments)) {
    dir = experiments[[i]]

    data <- try(read.nmr.directory.bruker.process(dir, ...))
    if(inherits(data, "try-error")) next

    time=as.numeric(data$date)
    if(!length(ppm_scale)) {
      ppm_scale=data$ppm
      t0=time
    }
    if(all(data$ppm == ppm_scale)) {
      columns[[i]]<-data$intensity
      names(columns)[[i]]<-time-t0
    } else {
      warning('ppm scale does not match for ', dir, ' (skipping)')
      next
    }
  }
  if(!length(columns)) stop('No data found!')
  nmrdata=nmr2d.data.object(ppm_scale,columns)
  #Rename columns
  names(nmrdata)[[1]] <- "ppm"
  attr(nmrdata, 'nuc1')<-attr(columns[[1]], 'nuc1')
  #return data
  return(nmrdata)
}

read.nmr.directory.bruker.process <- function(dir, process_no=1, ...) {
  if(!dir.exists(file.path(dir, 'pdata'))) {
    stop('Directory does not contain pdata: ',dir)
  }
  acqs = suppressWarnings(read.acqu(dir))
  dim=acqs$parmode +1
  path = file.path(dir, 'pdata', process_no, 'ascii-spec-scaled.txt')
  path2 = file.path(dir, 'pdata', process_no, 'ascii-spec.txt')
  if(dim==1 && file.exists(path)) {
    data = read.nmr.directory.text.1d.2col(path, skip=1)
  } else if(dim==1 && file.exists(path2)) {
    data = read.nmr.directory.text.1d.2col(path2, skip=1)
  } else {
    #Can't find any ascii data, so need to read the binary files here
    #See read.fid.R

    stop('Processing binary or multidimensional data is currently not supported, skipping ', dir)

    #OLD:
    if(dim==1) {
      stop("Cannot process raw data for 1D experiments, use topspin's export via File->Save as->Save data of currently displayed region in a text file instead. (This also allows for imaginary data).")
    } else if (dim==2) {
      data = (read.NMR2D.raw(path,acqs))
    } else {
      stop("Only 1D & 2D data are supported.")
    }
  }

  for(att in names(acqs)) {
    attr(data,att)<-acqs[[att]]
  }
  return(data)
}
