#' Export insitu NMR data
#'
#' @export
export.nmr2dinsitu.data.object <- function(data, path) {
  echem <- attr(data, "echem")
  attr(data, "echem") <- NULL
  class(data) <- class(data)[-1]
  export(data, path)
  echem_path <- paste0(tools::file_path_sans_ext(path), "-echem-data.csv")
  export(echem, echem_path)
}
