#' @author Josh Stratford, \email{}
#' ...
#' @import Plotting.Utils
NULL

.onLoad <- function(libname, pkgname){
  Plotting.Utils::create_data_type('nmr','ppm','',envir=asNamespace('NMR.Utils'))
  Plotting.Utils::create_data_type('nmr2d','ppm','',inherits='nmr',envir=asNamespace('NMR.Utils'))
  Plotting.Utils::create_data_type('nmr2dinsitu','ppm','',inherits=c('nmr2d','nmr'),envir=asNamespace('NMR.Utils'))
}

