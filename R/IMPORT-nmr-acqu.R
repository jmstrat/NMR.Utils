#' Read Acquisition Parameters
#'
#' This function reads acquisition pararameters for NMR Data.
#' @param dir Path to raw data directory (up to expNo)
#' @return A list of parameters
#' @export
#' @examples
#' read.acqu("/path/to/nmr/expName/expNo")
read.acqu <- function(dir) {
  path <- paste0(dir, "/acqus")
  if (file.exists(path)) {
    jms.classes::log.info("Reading acquisition parameters from %s", path)
    con <- file(path, open="r")
    head0 <- readLines(con)
    close(con)
    position <- grep("^\\$\\$", head0)[[1]]
    acqu <- list()
    acqu$file <- gsub("^\\$\\$ ", "", head0[[position + 1]])
    acqu$date <- as.POSIXct(sub("^\\$\\$ ([^[:space:]]* [^[:space:]]* [^[:space:]]*) .*$", "\\1", head0[[position]]))

    get_pat <- function(pat) {
      gsub(pat, "", head0[grepl(pat, head0)])
    }

    acqu$aq_mod     <- suppressWarnings(as.numeric(get_pat("^##\\$AQ_mod= ")))
    acqu$bytorda    <- suppressWarnings(as.numeric(get_pat("##\\$BYTORDA= ")))
    acqu$cnst       <- suppressWarnings(as.numeric(get_pat("##\\$CNST= ")))
    acqu$d          <- suppressWarnings(as.numeric(get_pat("##\\$D= ")))
    acqu$de         <- suppressWarnings(as.numeric(get_pat("##\\$DE= ")))
    acqu$decim      <- suppressWarnings(as.numeric(get_pat("##\\$DECIM= ")))
    acqu$digmod     <- suppressWarnings(as.numeric(get_pat("##\\$DIGMOD= ")))
    acqu$ds         <- suppressWarnings(as.numeric(get_pat("##\\$DS= ")))
    acqu$dspfvs     <- suppressWarnings(as.numeric(get_pat("##\\$DSPFVS= ")))
    acqu$fw         <- suppressWarnings(as.numeric(get_pat("##\\$FW= ")))
    acqu$gpx        <- suppressWarnings(as.numeric(get_pat("##\\$GPX= ")))
    acqu$gpy        <- suppressWarnings(as.numeric(get_pat("##\\$GPY= ")))
    acqu$gpz        <- suppressWarnings(as.numeric(get_pat("##\\$GPZ= ")))
    acqu$in_        <- suppressWarnings(as.numeric(get_pat("##\\$IN= "))) ### extra underscore in r
    acqu$inp        <- suppressWarnings(as.numeric(get_pat("##\\$INP= ")))
    acqu$l          <- suppressWarnings(as.numeric(get_pat("##\\$L= ")))
    acqu$nbl        <- suppressWarnings(as.numeric(get_pat("##\\$NBL= ")))
    acqu$ns         <- suppressWarnings(as.numeric(get_pat("##\\$NS= ")))
    acqu$o1         <- suppressWarnings(as.numeric(get_pat("##\\$O1= ")))
    acqu$parmode    <- suppressWarnings(as.numeric(get_pat("##\\$PARMODE= ")))
    acqu$pl         <- suppressWarnings(as.numeric(get_pat("##\\$PL= ")))
    acqu$l          <- suppressWarnings(as.numeric(get_pat("##\\$L= ")))
    acqu$pulprog    <- get_pat("##\\$PULPROG= ")
    acqu$pulprog    <- substr(acqu$pulprog, 2, nchar(acqu$pulprog) - 1)
    acqu$rg         <- suppressWarnings(as.numeric(get_pat("##\\$RG=")))
    acqu$sfo1       <- suppressWarnings(as.numeric(get_pat("##\\$SFO1=")))
    acqu$o1p        <- suppressWarnings(as.numeric(acqu$o1 / acqu$sfo1))
    acqu$sw         <- suppressWarnings(as.numeric(get_pat("##\\$SW=")))
    acqu$sw_h       <- suppressWarnings(as.numeric(get_pat("##\\$SW_h=")))
    acqu$td         <- suppressWarnings(as.numeric(get_pat("##\\$TD=")))
    acqu$td[[2]]    <- 1
    acqu$td[[3]]    <- 1
    acqu$td[[4]]    <- 1
    acqu$te         <- suppressWarnings(as.numeric(get_pat("##$TE=")))
    acqu$vdlist     <- get_pat("##\\$VDLIST= ")
    acqu$vdlist     <- substr(acqu$vdlist, 3, nchar(acqu$vdlist) - 1)
    acqu$vplist     <- get_pat("##\\$VPLIST= ")
    acqu$vplist     <- substr(acqu$vplist, 3, nchar(acqu$vplist) - 1)
    acqu$vtlist     <- get_pat("##\\$VTLIST=")
    acqu$vtlist     <- substr(acqu$vtlist, 3, nchar(acqu$vtlist) - 1)
    acqu$nuc1       <- get_pat("##\\$NUC1=")
    acqu$nuc1       <- substr(acqu$nuc1, 3, nchar(acqu$nuc1) - 1)
  } else {
    stop('ERROR: Could not find "acqus"')
  }

  if (acqu$parmode > 0) {
    path <- paste0(dir, "/acqu2s")
    if (file.exists(path)) {
      jms.classes::log.info("Reading acquisition parameters from %s", path)
      con <- file(path, open="r")
      head0 <- readLines(con)
      close(con)

      acqu$sw[[2]]   <- as.numeric(get_pat("##\\$SW="))
      acqu$sw_h[[2]] <- as.numeric(get_pat("##\\$SW_h="))
      acqu$td[[2]]   <- as.numeric(get_pat("##\\$TD="))

      if (acqu$parmode > 1) {
        path <- paste0(dir, "/acqu3s")
        jms.classes::log.info("Reading acquisition parameters from %s", path)
        if (file.exists(path)) {
          con <- file(path, open="r")
          head0 <- readLines(con)
          close(con)
          acqu$sw[[3]]   <- as.numeric(get_pat("##\\$SW="))
          acqu$sw_h[[3]] <- as.numeric(get_pat("##\\$SW_h="))
          acqu$td[[3]]   <- as.numeric(get_pat("##\\$TD="))

          if (acqu$parmode > 2) {
            path <- paste0(dir, "/acqu4s")
            jms.classes::log.info("Reading acquisition parameters from %s", path)
            if (file.exists(path)) {
              con <- file(path, open="r")
              head0 <- readLines(con)
              close(con)
              acqu$sw[[4]]   <- as.numeric(get_pat("##\\$SW="))
              acqu$sw_h[[4]] <- as.numeric(get_pat("##\\$SW_h="))
              acqu$td[[4]]   <- as.numeric(get_pat("##\\$TD="))
            } else {
              stop('ERROR: Could not find "acqu4s"')
            }
          }
        } else {
          stop('ERROR: Could not find "acqu3s"')
        }
      }
    } else {
      stop('ERROR: Could not find "acqu2s"')
    }
  }
  return(acqu)
}

#' Read Title for NMR file
#'
#' This function reads the title for NMR Data.
#' @param dir Path to raw data directory (up to expNo)
#' @return The title
#' @export
#' @examples
#' read.title("/path/to/nmr/expName/expNo")
read.nmr.title <- function(dir) {
  title <- ""
  fileName <- paste0(dir, "/pdata/1/title")
  if (file.exists(fileName)) {
    jms.classes::log.info("Reading title from %s", fileName)
    title <- readChar(fileName, file.info(fileName)$size)
  } else {
    jms.classes::log.warn('Unable to find title for NMR data at "%s"', dir)
  }
  return(title)
}
