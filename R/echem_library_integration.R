#' Allow echem library to plot NMR data
#'
#' This function is used to integrate NMR plotting into the echem library.
#' @export
setup_NMR_echem_integration <- function() {
  if(load_or_install('Echem',optional=TRUE)) {
    .inset_ex_situ_into_echem()
  }}

#' @export
.plot_ex_situ_nmr_echem <- function(cellids,data_file,acqus_file_directory,mass,  ...) { plot_ex_situ_nmr_files(files=data_file,acqus_dirs=acqus_file_directory, masses=mass, ...)}

.inset_ex_situ_into_echem <- function() {
  add_data_handler(type="ex-situ-nmr",
                   name="Ex Situ NMR",
                   operando=FALSE,
                   lib='NMR.Utils',
                   info_func=character(),
                   process_func=character(),
                   plot_func='.plot_ex_situ_nmr_echem',
                   parameter_names=list('data_file','acqus_file_directory','mass'),
                   parameter_types=list('file','directory','character')
  )
}
