#' Start a time remaining indicator
#'
#' This function starts a time remaining indicator.
#' @param max_scan The number of scans that will be performed during the time the indicator is shown.
#' @return A function to be called with the current scan number
#' @export
#' @examples
#' time_remaining_start(10)
#' f=time_remaining_start(10)
#' for(i in 1:10) {
#'   Sys.sleep(1)
#'   f(i)
#'   Sys.sleep(1)
#'   f()
#' }
#' @keywords internal
time_remaining_start <- function(max_scan) {
  cat("Calculating time remaining")
  func=time_remaining_print
  current_time=as.numeric(Sys.time())
  timeEnv=new.env()
  assign("start", current_time, envir = timeEnv)
  assign("current_scan_start", current_time, envir = timeEnv)
  assign("max_scan", max_scan, envir = timeEnv)
  assign("last_scan_printed", -1, envir = timeEnv)
  assign("current_scan", -1, envir = timeEnv)
  formals(func)['envir']=list(timeEnv)
  return(func)
}

#' Iterate a time remaining indicator
#'
#' This function iterates a time remaining indicator. It should not be called manually, rather the function returned from \code{\link[NMR.Utils]{time_remaining_start}} should be used.
#' @param current_scan The current scan number.
#' @return The environment containing the timing parameters.
#' @export
#' @examples
#' time_remaining_start(10)
#' f=time_remaining_start(10)
#' for(i in 1:10) {
#'   Sys.sleep(1)
#'   f(i)
#'   Sys.sleep(1)
#'   f()
#' }
#' @keywords internal
time_remaining_print <-function(current_scan=NULL,envir=NULL) {
  if(any(is.null(envir))) {
    return()
  }
  if(is.null(current_scan)) {
    current_scan=get("current_scan", envir = envir)
  } else {
    assign("current_scan", current_scan, envir = envir)
  }

  current_time=as.numeric(Sys.time())

  start=get("start", envir = envir)
  current_scan_start=get("current_scan_start", envir = envir)
  max_scan=get("max_scan", envir = envir)
  last_scan_printed=get("last_scan_printed", envir = envir)

  if(!current_scan==last_scan_printed)
  {
    assign("current_scan_start", current_time, envir = envir)
    current_scan_start=current_time
  }
  assign("last_scan_printed", current_scan, envir = envir)

  if(current_scan<=1)
    return()

  if(current_scan == max_scan) {
    cat(paste(c("\r [",current_scan,"/",max_scan,"] Done!                                                                                "),sep='',collapse=''))
    return()
  }

  elapsed_current_scan=current_time-current_scan_start

  elapsed=current_scan_start-start
  elapsed_average=elapsed/(current_scan-1)

  remaining=as.integer(elapsed_average*(max_scan-current_scan+1)-elapsed_current_scan)

  d=as.integer(remaining/(24*60*60))
  h=as.integer((remaining/(60*60))%%24)
  m=as.integer((remaining/60)%%60)
  s=remaining%%60
  d_txt="s"
  if(d==1)
    d_txt=""
  h_txt="s"
  if(h==1)
    h_txt=""
  m_txt="s"
  if(m==1)
    m_txt=""
  s_txt="s"
  if(s==1)
    s_txt=""

  if(!d==0) {
    cat(paste(c("\r [",current_scan,"/",max_scan,"] ",d," day",d_txt,", ",h," hour",h_txt,", ",m," minute",m_txt," and ",s," second",s_txt," remaining.                                                                    "),sep='',collapse=''))
  } else if(!h==0) {
    cat(paste(c("\r [",current_scan,"/",max_scan,"] ",h," hour",h_txt,", ",m," minute",m_txt," and ",s," second",s_txt," remaining.                                                                    "),sep='',collapse=''))
  } else if(!m==0) {
    cat(paste(c("\r [",current_scan,"/",max_scan,"] ",m," minute",m_txt," and ",s," second",s_txt," remaining.                                                                    "),sep='',collapse=''))
  } else {
    cat(paste(c("\r [",current_scan,"/",max_scan,"] ",s," second",s_txt," remaining.                                                                    "),sep='',collapse=''))
  }
}
