#' Phase NMR Data
#'
#' This function phases (complex) NMR data.
#' @param data A (complex) dataframe of NMR data
#' @param p0 0th-order phase correction coefficient
#' @param p1 1st-order phase correction coefficient
#' @param pivot Pivot point for 2nd-order phase correction coefficient (ppm)
#' @return A data frame containing the phased NMR data (ppm,intensity1, ...)
#' @export
#' @examples
#' phase(data,45,0,0)

phase <- function(data,p0,p1,pivot) {
  ppm=data[,1]
  pdata=data[,2:ncol(data)]
  ns=dim(pdata)
  if(is.null(ns))
    ns=c(length(pdata))
  n_points=ns[[1]]
  pivot_points=which(abs(ppm-pivot)==min(abs(ppm-pivot)))[[1]]
  x=seq(1,n_points,length.out=n_points)-pivot_points
  phi=p0/180*pi+p1/180*pi*x/1000
  phas_vector=complex(real=cos(phi),imaginary=-sin(phi))
  result=pdata*phas_vector
  data[,2:ncol(data)]=result
  return(data)
}

#' Autophase NMR Data with constraints (using a VERY CRUDE method)
#'
#' This function phases (complex) NMR data.
#' @param spectrum A (complex) dataframe of NMR data (single scan)
#' @param p0_optim_x_range x range over which to optimise the p0 value (ppm)
#' @param p1_optim_x_range x range over which to optimise the p1 value (ppm)
#' @param pivot Pivot point for 2nd-order phase correction coefficient (ppm)
#' @param p0_optim_range range over which the p0 value can vary
#' @param p1_optim_range range over which the p1 value can vary
#' @return A data frame containing the phased NMR data (ppm,intensity)
#' @export
#' @examples
#' apk(data,c(-200,200),c(1000,1300),0,c(-50,50),c(-5,0))
apk <-function(spectrum,p0_optim_x_range,p1_optim_x_range,pivot,p0_optim_range,p1_optim_range) {
  ppm=spectrum[,1]
  p0_xmin_points=which(abs(ppm-p0_optim_x_range[[1]])==min(abs(ppm-p0_optim_x_range[[1]])))[[1]]
  p0_xmax_points=which(abs(ppm-p0_optim_x_range[[2]])==min(abs(ppm-p0_optim_x_range[[2]])))[[1]]
  p1_xmin_points=which(abs(ppm-p1_optim_x_range[[1]])==min(abs(ppm-p1_optim_x_range[[1]])))[[1]]
  p1_xmax_points=which(abs(ppm-p1_optim_x_range[[2]])==min(abs(ppm-p1_optim_x_range[[2]])))[[1]]
  optim_f0 <-function(x) {
    real_phased=Re(phase(spectrum,x,0,0)[p0_xmin_points:p0_xmax_points,2])
    sum(real_phased)
  }
  p0=(optimise(optim_f0,lower=p0_optim_range[[1]],upper=p0_optim_range[[2]],maximum=T)$maximum)
  optim_f <-function(x) {
    real_phased=Re(phase(spectrum,p0,x,pivot)[p1_xmin_points:p1_xmax_points,2])
    sum(real_phased)
  }
  p1=optimise(optim_f,lower=p1_optim_range[[1]],upper=p1_optim_range[[2]],maximum=T)$maximum
  phase(spectrum,p0,p1,pivot)
}

#' Autophase NMR Data with constraints for a pseudo 2D dataset (e.g. in-situ echem). Uses (\code{\link[NMR.Utils]{apk}})
#'
#' This function phases (complex) NMR data.
#' @param spectra (complex) dataframe containing spectra
#' @param p0_optim_x_range x range over which to optimise the p0 value (ppm)
#' @param p1_optim_x_range x range over which to optimise the p1 value (ppm)
#' @param pivot Pivot point for 2nd-order phase correction coefficient (ppm)
#' @param p0_optim_range range over which the p0 value can vary
#' @param p1_optim_range range over which the p1 value can vary
#' @return A data frame containing the phased NMR data (ppm,intensity1, ...)
#' @export
#' @examples
#' apkpseudo2d(data,c(-200,200),c(1000,1300),0,c(-50,50),c(-5,0))
apkpseudo2d <-function(spectra,p0_optim_x_range,p1_optim_x_range,pivot,p0_optim_range,p1_optim_range) {
  for(i in 2:ncol(spectra)) {
    spectra[,i]=apk(spectra[,c(1,i)],p0_optim_x_range,p1_optim_x_range,pivot,p0_optim_range,p1_optim_range)[,2]
  }
  return(spectra)
}

#' Removes imaginary component from NMR data
#'
#' @param spectra A (complex) dataframe of NMR data
#' @return A data frame containing the real NMR data (ppm,intensity1, ...)
#' @export
#' @examples
#' makeReal(data)
makeReal <-function(spectra) {
    nam=names(spectra)
    newdf=as.data.frame(lapply(spectra,Re))
    names(newdf)<-nam
    return(newdf)
}

#' Generates time offsets if spectra were acquired without ATM
#'
#' @param spectra A dataframe of NMR data
#' @param scan_length Length of a single scan (hours)
#' @param initial_offset Offset of 1st scan (hours)
#' @return A vector of offsets
#' @export
#' @examples
#' noATMoffsets(spectra,0.5)
noATMoffsets <-function(spectra,scan_length,initial_offset=0) {
    n=ncol(spectra)-1
    return(seq(0:n)*scan_length+initial_offset)
}

#' Stores the time offsets in the data frame
#'
#' @param spectra A dataframe of NMR data
#' @param offsets A vector of offsets
#' @return An idential data frame whose names correspond to the offsets
#' @export
#' @examples
#' storeOffsets(data,offsets)
storeOffsets <-function(spectra,offsets) {
    lenO=length(offsets)
    lenS=ncol(spectra)-1
    if(lenO<lenS) {
      offsets=append(offsets,(lenO+1):lenS)
    } else if(lenS<lenO) {
      offsets=offsets[c(1:lenS)]
    }
    names(spectra)[c(2:(lenS+1))]<-offsets
    return(spectra)
}

#' Reduces the number of scans in the data frame
#'
#' @param spectra A dataframe of NMR data
#' @param scan_range A vector of scans to include c(first, last)
#' @return An idential data frame whose scans are limited to those in scan_range
#' @export
#' @examples
#' reduceScans(spectra,c(1,50))
reduceScans <-function(spectra,scan_range) {
    scan_range=scan_range+1
    if(scan_range[[1]]<2)
        scan_range[[1]]=2
    n=ncol(spectra)
    if(scan_range[[2]]>n)
        scan_range[[2]]=n
    #Keep attributes
    newspectra=spectra[,c(1,scan_range[[1]]:scan_range[[2]])]
    special_attrs=c('class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp')
    oldAtts=attributes(spectra)
    oldAtts=oldAtts[!names(oldAtts)%in%special_attrs]
    attributes(newspectra)[names(oldAtts)]<-oldAtts
    return(newspectra)
}
