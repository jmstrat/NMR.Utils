#' @author Josh Stratford, \email{}
#' ...
#' @import Plotting.Utils
NULL

.onLoad <- function(libname, pkgname){
  jms.classes::create_data_type('nmr','ppm','',envir=asNamespace('NMR.Utils'))
  jms.classes::create_data_type('nmr2d','ppm','',inherits='nmr',envir=asNamespace('NMR.Utils'))
  jms.classes::create_data_type('nmr2dinsitu','ppm','',inherits=c('nmr2d','nmr'),envir=asNamespace('NMR.Utils'))
}

