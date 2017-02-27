#' Load packages
#'
#' This function will load a package, or install it if it isn't present. (Probably against all sorts of programming guidelines, but makes everything easier to install and run...)
#' @param package The package name
#' @param optional If FALSE then an error will be raised if the package cannot be loaded
#' @param github If installing from cran fails try this github repository instead (see \code{\link[devtools]{install_github}})
#' @return TRUE if the package was successfully loaded. FALSE if the package was unable to be loaded and optional=TRUE. Raises an error otherwise.
#' @examples
#' load_or_install("baseline")
#' @keywords internal
load_or_install <- function(package,optional=FALSE, github=NA)
{
  if (!require(package,character.only = TRUE))
  {
    flog.warn('The package "%s was not found, attempting to install...',package)
    install.packages(package,dependencies=TRUE, repos="http://cran.rstudio.com/")
    if(!require(package,character.only = TRUE)) {
      #Check if available on github
      if(!is.na(github)) {
        flog.warn('Package "%s was not found on cran, checking github...',package)
        if(load_or_install('devtools',optional=TRUE) && tryCatch({devtools::install_github(github)
          return(TRUE)}, error = function(e) return(FALSE))) {
          if(require(package,character.only = TRUE)) return(invisible(TRUE))
        }
      }
      if(!optional) stop("Package '",package,"' not found",call. = FALSE) else flog.warn('The optional package "%s was not found.',package)
      return(invisible(FALSE))
    }
  }
  return(invisible(TRUE))
}
