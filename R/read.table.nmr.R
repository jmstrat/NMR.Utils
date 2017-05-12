#' Read a table as an NMR data object
#'
#' @param ... parameters are passed to \code{\link{read.table}}
#' @return An NMR data object containing the data
#' @keywords internal
read.table.nmr <- function(...) {
  df=read.table(...)
  return(make.nmr.data.object(df))
}

#' Make a new NMR data object
#'
#' @param ... parameters are passed to \code{\link{data.frame}}
#' @return An NMR data object
#' @keywords internal
nmr.data.object <- function(...) {
  return(make.nmr.data.object(data.frame(...)))
}

#' Convert a data.frame to an NMR data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @keywords internal
make.nmr.data.object <- function(x) {
  if(!is.data.frame(x)) {
    stop("make.nmr.data.object can only be called with a data.frame")
  }
  attr(x, "class") <- c("nmr.data.object", "data.frame")
  attr(x,'file_type')<-NULL
  attr(x,'data_type')<-NULL
  attr(x,'y_type')<-'Unknown'
  attr(x,'x_type')<-'Unknown'
  attr(x,'x_column')<-1
  attr(x,'y_column')<-2
  return(x)
}

#' Convert an NMR data object to a 2D NMR data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @keywords internal
make.nmr.2Ddata.object <- function(x) {
  if(is.nmr.data.object(x)) {
    attr(x, "class") <- c("nmr.2Ddata.object","nmr.data.object", "data.frame")
  } else {
    stop("make.nmr.2ddata.object can only be called with an nmr.data.object")
  }
  return(x)
}

#' Check if an object is an NMR data object
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @keywords internal
is.nmr.data.object <- function(x) {
  return(inherits(x,"nmr.data.object"))
}

#' Check if an object is a 2D NMR data object
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @keywords internal
is.nmr.2Ddata.object <- function(x) {
  return(inherits(x,"nmr.2Ddata.object"))
}

#' Retain attributes upon subsetting
`[.nmr.data.object` <- function (x, ...) {
  r <- NextMethod("[")
  special_attrs=c('class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp')
  oldAtts=attributes(x)
  oldAtts=oldAtts[!names(oldAtts)%in%special_attrs]
  attributes(r)[names(oldAtts)]<-oldAtts
  return (r)
}
