#' Plot ex situ NMR data
#'
#' This function is used to plot ex situ NMR data.
#' @param x An nmr.data.object
#' @param ... Additional parameters passed to \code{\link[jms.classes]{plot.jms.data.object}}
#' @export
plot.nmr.data.object <- function(x,...,axes=1,xlim=NULL) {
  if(any_complex(x)) x = makeReal(x)
  if(any(is.null(xlim))) xlim=range(x[,jms.classes::xcol(x)][is.finite(x[,jms.classes::xcol(x)])])
  xlim=rev(range(xlim))
  NextMethod(x,axes=axes,xlim=xlim,div=2,...)
}

#' @export
lines.nmr.data.object <- function(x,...) {
  if(any_complex(x)) x = makeReal(x)
  # Normalise
  m=attr(x,'mass')
  if(is.null(m)) m=1
  ns=attr(x,'mass')
  if(is.null(ns)) ns=1
  x = x / m / ns
  NextMethod(x,...)
}

#' Plot ex situ NMR data
#'
#' This function is used to plot ex situ NMR data.
#' @param x An nmr.data.object
#' @param ... Additional parameters passed to \code{\link[jms.classes]{plot.jms.data.object}}
#' @export
iplot.nmr.data.object <- function(...,axes=1,offset=NULL,xlim=NULL,ylim=NULL,y2lim=NULL,xlab=NULL,ylab=NULL,y2lab=NULL,col=NULL,lwd=NULL,pch=NULL,labels=NULL,group=NULL) {
  data=list(...)
  warnedM=warnedNS=F

  if(length(data)>1) {
    for(i in 1:length(data)) {
      m=attr(data[[i]],'mass')
      if(is.null(m)) {
        m=1
        if(!warnedM) {
          warning('Data are not normalised by mass')
          warnedM=T
        }
      }
      m=as.numeric(m)
      ns=attr(data[[i]],'ns')
      if(is.null(ns)) {
        ns=1
        if(!warnedNS) {
          warning('Data are not normalised by ns')
          warnedNS=T
        }
      }
      ns=as.numeric(ns)
      if(any_complex(data[[i]])) data[[i]] = makeReal(data[[i]])
      data[[i]]=data[[i]] / m / ns
    }
  } else {
    if(any_complex(data[[1]])) data[[1]] = makeReal(data[[1]])
  }
  data=jms.classes::combine(unname(data),interpolate=TRUE)
  if(any(is.null(xlim))) {
    xlim=range(data[,jms.classes::xcol(data)][is.finite(data[,jms.classes::xcol(data)])])
  }
  xlim=rev(range(xlim))
  if(!length(labels)) {
    dots <- substitute(list(...))[-1]
    labels=c(sapply(dots, deparse))
    if(object.size(labels)>1000) labels<-NULL
  }

  args=list(data, axes=axes, offset=offset, xlim=xlim,
            ylim=ylim, y2lim=y2lim, xlab=xlab, ylab=ylab,
            y2lab=y2lab, col=col, lwd=lwd, pch=pch,
            labels=labels, group=group)


  do.call(jms.classes:::iplot.jms.data.object, args[!sapply(args,is.null)])
  # NextMethod(data, axes=axes, xlim=xlim)
}
