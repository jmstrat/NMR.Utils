read.nmr.directory.bruker.experiment <- function(path, experiment_nos=NA, ...) {
  jms.classes::log.debug('Treating %s as a Bruker experiment directory', path)
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

read.nmr.directory.bruker.process <- function(dir, process_no=1, imaginary=FALSE, ...) {
  jms.classes::log.debug('Treating %s as a Bruker process directory', dir)
  if(!dir.exists(file.path(dir, 'pdata'))) {
    stop('Directory does not contain pdata: ',dir)
  }
  acqs = suppressWarnings(read.acqu(dir))
  proc = file.path(dir, 'pdata', process_no)
  proc = read.proc(proc)

  dim=acqs$parmode +1
  path = file.path(dir, 'pdata', process_no, 'ascii-spec-scaled.txt')
  path2 = file.path(dir, 'pdata', process_no, 'ascii-spec.txt')
  if(dim==1 && file.exists(path)) {
    jms.classes::log.info('Reading processed data from %s', path)
    data = read.nmr.directory.text.1d.2col(path, skip=1, ',')

  } else if(dim==1 && file.exists(path2)) {
    jms.classes::log.info('Reading processed data from %s', path2)
    data = read.nmr.directory.text.1d.2col(path2, skip=1, ',')

    scale = tryCatch({
      2^(-proc$nc_proc)
    }, error = function(e) {
      1
    })
    jms.classes::log.debug('Rescaling ascii-spec data using a factor of %s', scale)
    data = data / scale

  } else {
    #Can't find any ascii data, so need to read the binary files here
    if(dim==1) {
      real = file.path(dir, 'pdata', process_no, '1r')

      if(file.exists(real)) {
        imag = file.path(dir, 'pdata', process_no, '1i')
        if(!imaginary || !file.exists(imag)) {
          imag = NA
        }
        jms.classes::log.debug('Found Bruker processed data')

        data = read.nmr.bruker.processed.binary.1d(real, imag, acqs, proc)
      } else {
        stop('Only processed data are supported.')
      }
    } else if (dim==2) {
      real = file.path(dir, 'pdata', process_no, '2rr')
      if(file.exists(real)) {
        imag = file.path(dir, 'pdata', process_no, '2ii')
        if(!imaginary || !file.exists(imag)) {
          imag = NA
        }
        jms.classes::log.debug('Found Bruker processed data')

        data = read.nmr.bruker.binary.2d(real, imag, acqs, proc)
      } else {
        stop('Only processed data are supported.')
      }
    } else {
      stop("Only 1D & 2D data are supported.")
    }
  }

  try({
    sfo1 = acqs$sfo1
    sf = proc$proc$sf
    car = (sfo1-sf)*1e6/sfo1
    attr(data, 'Carrier Frequency ppm')<-car
  }, silent=T)

  try({
    attr(data, 'Title')<-read.nmr.title(dir)
  }, silent=T)

  for(att in names(acqs)) {
    attr(data,att)<-acqs[[att]]
  }
  attr(data,'proc')<-proc
  return(data)
}
