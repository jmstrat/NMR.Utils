#' Read ATMC log file
#'
#' Calculates offsets in hours from first scan based on ATMC log file
#' @param log_file_path Path to txt file
#' @param version Version of the log file (0 == orignal backup log -- no-longer used. 1 == Version prior to March 2016 upgrade.)
#' @param experimentNumber Index of the experiment in the current log file (currently only used if version=0)
#' @return A vector of offsets giving the number of hours after the first scan that each scan was started.
#' @export
#' @examples
#' read.ATMC("/path/to/file.txt")
read.ATMC <- function(log_file_path, version=NA, experimentNumber=1) {
  if (is.na(version)) {
    return(read.ATMC_generic(log_file_path, "(Experiment in progress)|(Send Bruker ready)"))
  }

  if (version == 2) {
    return(read.ATMC2(log_file_path))
  } else if (version == 1) {
    return(read.ATMC1(log_file_path))
  } else if (version == 0) {
    return(read.ATMC0(log_file_path, experimentNumber))
  } else {
    stop(paste0("Unable to process ATMC log file. Version not known (Version ", version, ")."))
  }
}

#' Read ATMC log file
#'
#' Calculates offsets in hours from first scan based on ATMC log file
#' @param log_file_path Path to txt file
#' @param searchString String indicating the start of a new scan
#' @return A vector of offsets giving the number of hours after the first scan that each scan was started.
#' @examples
#' read.ATMC_generic("/path/to/file.txt", "search_string")
#' @keywords internal
read.ATMC_generic <- function(log_file_path, searchString) {
  # Read ATMC log file
  # Open file for reading
  con <- file(log_file_path, open="r")
  # Read lines
  lines <- readLines(con, n=-1)
  # Close file
  close(con)

  tDone_lines <- grep(searchString, lines)
  times <- gsub(paste0("([[:digit:]\\.:[:space:]]*)", searchString, ".*", collapse=""), "\\1", lines[tDone_lines])
  times <- lapply(times, function(x) as.POSIXct(x, format="%d.%m.%Y  %H:%M:%S"))

  t0 <- times[[1]]
  offsets <- c()
  for (t in times) {
    offsets <- append(offsets, as.double(difftime(t, t0, units="hours")))
  }
  return(offsets)
}

#' Read ATMC (version 0 -- backup) log file
#'
#' Calculates offsets in hours from first scan based on ATMC log file
#' @param log_file_path Path to txt file
#' @param experimentNumber Index of the experiment in the current log file
#' @return A vector of offsets giving the number of hours after the first scan that each scan was started.
#' @examples
#' read.ATMC0("/path/to/file.txt")
read.ATMC0 <- function(log_file_path, version=1, experimentNumber=1) {
  # Read ATMC log file
  # Open file for reading
  con <- file(log_file_path, open="r")
  # Read lines
  lines <- readLines(con, n=-1)
  # Close file
  close(con)

  experiment_lines <- grep("Neuer Experiment Bruker beginnt", lines)

  eCount <- length(experiment_lines)

  if (experimentNumber == 1) {
    lines <- lines[c(1:experiment_lines[[1]])]
  } else if (experimentNumber == eCount) {
    lines <- lines[c(experiment_lines[[eCount]]:length(lines))]
  } else {
    lines <- lines[c(experiment_lines[[experimentNumber]]:experiment_lines[[experimentNumber + 1]])]
  }

  tDone_lines <- grep("Experiment Bruker ended.", lines)
  times <- gsub("([[:digit:]\\.:[:space:]]*)\\(*", "\\1", lines[tDone_lines])
  times <- lapply(times, function(x) as.POSIXct(x, format="%Y.%m.%d %H:%M:%S"))

  t0 <- times[[1]]
  offsets <- c()
  for (t in times) {
    offsets <- append(offsets, as.double(difftime(t, t0, units="hours")))
  }
  return(offsets)
}


#' Read ATMC (version 1) log file
#'
#' Calculates offsets in hours from first scan based on ATMC log file
#' @param log_file_path Path to txt file
#' @return A vector of offsets giving the number of hours after the first scan that each scan was started.
#' @examples
#' read.ATMC1("/path/to/file.txt")
read.ATMC1 <- function(log_file_path) read.ATMC_generic(log_file_path, "Experiment in progress")


#' Read ATMC (version 2) log file
#'
#' Calculates offsets in hours from first scan based on ATMC log file
#' @param log_file_path Path to txt file
#' @return A vector of offsets giving the number of hours after the first scan that each scan was started.
#' @examples
#' read.ATMC2("/path/to/file.txt")
read.ATMC2 <- function(log_file_path) read.ATMC_generic(log_file_path, "Send Bruker ready")
