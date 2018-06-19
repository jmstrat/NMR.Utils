#' Plot In situ NMR data
#'
#' This function plots in-situ NMR data.
#' @param nmr In situ nmr data
#' @param use.default.layout If TRUE, a side-by-side 3:1 layout will be used for NMR-Echem.
#'                           If FALSE, \code{\link[graphics]{layout}} must be called prior to calling \code{plot}
#'                           Plot #1 will be used for NMR, #2 for echem
#' @param separation Gap between plots (in inches)
#' @param separation.pos Where to put the separation -- vector c(nmr, echem)
#'                       where nmr,echem are one of 1=bottom, 2=left, 3=top, 4=right
#' @param nmr.func Function to plot NMR data (default \code{\link[NMR.Utils]{plot.nmr2d.data.object}})
#' @param echem.func Function to plot echem data (default \code{\link[NMR.Utils]{plot_echem_vertical}})
#' @param ... Further parameters will be passed to the plotting methods
#' @return Returns a function to convert voltage, time into x,y values for the echem plot
#' @export
#' @examples
#' plot(data)
#'
#' rescale <- plot(data)
#' time = c(1, 2)
#' V = Plotting.Utils::nearest(echem, Test_Time.s.=time*3600)$Voltage.V
#' points(rescale(V, time), col='red')
plot.nmr2dinsitu.data.object <-function(nmr, use.default.layout=TRUE, separation=0.1, separation.pos=c(4, 2), nmr.func=NULL, echem.func=NULL, ...) {

  if(is.null(nmr.func)) nmr.func = getS3method('plot', 'nmr2d.data.object')
  if(is.null(echem.func)) echem.func = plot_echem_vertical

  echem=attr(nmr,'echem')
  if(!Echem.Data::is.echem.data.object(echem)) stop('Echem data is invalid')

  args=list(...)
  echem_args=args[names(args) %in% names(formals(echem.func))]
  NMR_args=args[names(args) %in% names(formals(nmr.func))]

  echem_args=append(list(echem), echem_args)
  NMR_args=append(list(nmr), NMR_args)

  margins=par('mai')

  #adjacent horizontal plot width ratio 3:1
  if(use.default.layout) layout(matrix(c(1, 2), 1, 2), widths=c(3,1))


  set_margins <- function(n) {
    if(!is.na(separation)) {
      pos = separation.pos[[n]]
      if(pos == 1) {
        mai = c(separation / 2, margins[2:4])
      } else if(pos == 4) {
        mai = c(margins[1:3], separation / 2)
      } else {
        mai=c(margins[1:(pos-1)], separation / 2, margins[(pos+1):4])
      }
      par(mai=mai)
    }
  }

  #set margins for NMR plot (#1)
  set_margins(1)

  #plot NMR data and recieve time and offset of first and last scan plotted
  align=do.call(nmr.func,NMR_args)
  echem_args=append(echem_args,list(time_start=align$time_scan_1,
                                    time_end=align$time_scan_last,
                                    yrange=align$yrange,
                                    offset_start=align$offset_scan_1,
                                    offset_end=align$offset_scan_last))
  #Get (last) unique
  echem_args<-echem_args[length(names(echem_args))-match(unique(names(echem_args)),rev(names(echem_args)))+1]

  #set margins for Echem plot (#2)
  set_margins(2)

  #Plot echem
  rescale_func <- do.call(echem.func,echem_args)

  invisible(rescale_func)

  # Note we cannot restore par / layout as then the rescale function wouldn't work
}


#' Plot electrochemisty data (vertically)
#'
#' This function plots in-situ echem data.
#' @param data Echem data
#' @param V_range Voltage range of echem data to plot
#' @param time_start,time_end Range of time to plot
#' @param y_range Y range to scale the data to
#' @param offset_start,offset_end y values of 1st and last data point
#' @param xaxismline Margin line on which to draw the x axis
#' @param xaxislabelmline Margin line on which to draw the x axis title
#' @return Returns a function to convert voltage, time into x,y values for the plot
#' @export
plot_echem_vertical <- function(data, V_range=NA, time_start=0, time_end=Inf,
                                yrange=c(0,1), offset_start=0, offset_end=1,
                                xaxismline=-0.8, xaxislabelmline=1.1) {
  if(nrow(data) == 0) {
    data=data.frame(Test_Time.s. = NA, Voltage.V. = NA)
  } else {
    #restrict echem to region requested
    data=data[data$Test_Time.s. >= time_start * 3600 & data$Test_Time.s. <= time_end * 3600, ]
  }

  if(time_end==Inf) time_end=max(data$Test_Time.s.) else time_end = time_end * 3600
  time_start = time_start * 3600

  if(any(is.na(V_range))) V_range=c(min(data$Voltage.V.), max(data$Voltage.V.))

  total_y2_range = (time_end-time_start) / 3600 / ((offset_end - offset_start) / (yrange[[2]]-yrange[[1]]))
  time_zero_as_fraction_of_yrange = (yrange[[1]]) / (yrange[[2]] - yrange[[1]])
  time_zero_in_y2 = time_zero_as_fraction_of_yrange * total_y2_range

  Plotting.Utils::pretty_plot(xlim=c(-V_range[[2]],-V_range[[1]]), ylim=yrange, axes=c(1,4),
                              y2lim=c(time_zero_in_y2, total_y2_range-time_zero_in_y2),
                              frac=TRUE, div=c(2,5), invertX=TRUE,
                              upperTickLimit=c(NA,NA,time_end/3600),
                              lowerTickLimit=c(NA,NA,time_start/3600),
                              line=xaxismline,labline=xaxislabelmline,
                              xlab=expression("Voltage / V"), y2lab="Time / h")

  #plot echem
  # DO NOT ADD offset_start -- echem always starts at time 0!
  lines(-data$Voltage.V.,data$Test_Time.s. * (offset_end-offset_start) / (time_end-time_start))

  # Return rescale function
  function(x, y) {
    xy = xy.coords(x, y)
    data.frame(x=-xy$x, y=xy$y * 3600 * (offset_end-offset_start) / (time_end-time_start))
  }
}
