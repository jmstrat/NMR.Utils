#' Allow echem library to plot NMR data
#'
#' This function is used to integrate NMR plotting into the echem library.
#' @export
setup_NMR_echem_integration <- function() {
  if(load_or_install('Echem',optional=TRUE)) {
    .inset_ex_situ_into_echem()
  }
}

#' @export
.plot_ex_situ_nmr_echem <- function(cellids,data_file,acqus_file_directory,mass,  ...) { plot_ex_situ_nmr_files(files=data_file,acqus_dirs=acqus_file_directory, masses=mass, ...)}

.inset_ex_situ_into_echem <- function() {
  add_data_handler(type="ex-situ-nmr",
                   name="Ex Situ NMR",
                   operando=FALSE,
                   lib='NMR.Utils',
                   info_func='.info_for_nmr_echem',
                   process_func=character(),
                   plot_func='.plot_ex_situ_nmr_echem',
                   parameter_names=list('data_file','acqus_file_directory','mass'),
                   parameter_types=list('file','directory','character')
  )
}

#' @export
.info_for_nmr_echem <- function(cellids,data_file,acqus_file_directory,mass,  ...) {
  info=list()
  get_record_id <-function(recordID) {
    recordID=as.character(recordID)
    if(recordID %in% names(info)) {
      record_i=sub('^[[:digit:]]*-?([[:digit:]]*)','\\1',recordID)
      record_cell=sub('^([[:digit:]]*)-?[[:digit:]]*','\\1',recordID)
      if(record_i=="") record_i=1
      record_i=record_i+1
      recordID=paste0(record_cell,'-',as.character(record_i))
      recordID=get_record_id(recordID)
    }
    recordID
  }
  for(i in 1:length(cellids)) {
    if(!all(is.na(acqus_file_directory[[i]]))) {
      summary=basename(dirname(acqus_file_directory[[i]]))
      information=read.nmr.title(acqus_file_directory[[i]])
      acqudate=read.acqu(acqus_file_directory[[i]])$date
    } else {
      summary=NA
      information=NA
      acqudate=as.Date(as.POSIXct(0, origin = '1970-01-01'))
    }
    record=list(summary=summary,date=acqudate,information=information)
    recordID=get_record_id(cellids[[i]])
    info[[recordID]]=record
  }
  info
}
