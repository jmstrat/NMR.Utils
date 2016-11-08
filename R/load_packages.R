#' Load packages
#'
#' This function will load a package, or install it if it isn't present. (Probably against all sorts of programming guidelines, but makes everything easier to install and run...)
#' @param x A (complex) dataframe of NMR data
#' @return None
#' @examples
#' load_or_install("baseline")
#' @keywords internal
load_or_install <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    warning(paste0('The package "',x,'" was not found, attempting to install...'))
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}