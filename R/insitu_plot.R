#' Plot In situ NMR data
#'
#' This function plots in-situ NMR data.
#' @param nmr In situ nmr data
#' @param widths Ratio of NMR to echem plot sizes
#' @param ... Further parameters will be passed to \code{\link[NMR.Utils]{plot.nmr2d.data.object}} or \code{\link[NMR.Utils]{plot_echem_vertical}}
#' @export
#' @examples
#' plot(data)
plot.nmr2dinsitu.data.object <-function(nmr,widths=c(3,1), ...) {
  echem=attr(nmr,'echem')
  if(!Echem.Data::is.echem.data.object(echem)) stop('Echem data is invalid')
  args=list(...)
  echem_args=args[names(args) %in% names(formals(plot_echem_vertical))]
  NMR_args=args[names(args) %in% names(formals(getS3method('plot', 'nmr2d.data.object')))]

  echem_args=append(list(echem,tick_size_multiplier=widths[[1]]/widths[[2]]),echem_args)
  NMR_args=append(list(nmr,show_RH_Tick=FALSE),NMR_args)

  #Store par settings
  original_par=par('mar','xpd','omi')

  margins=par('mai')

  #adjacent horizontal plot width ratio 5:1
  layout(matrix(c(1, 2), 1, 2, byrow=F), widths=widths)
  #set margins for LH plot to leave no margin on RHS
  par(mai=c(margins[1:3],0.05),xpd=FALSE)
  #plot NMR data and recieve time and offset of first and last scan plotted
  align=do.call(getS3method('plot', 'nmr2d.data.object'),NMR_args)
  echem_args=append(echem_args,list(time_start=align$time_scan_1,
                                    time_end=align$time_scan_last,
                                    yrange=align$yrange,
                                    offset_start=align$offset_scan_1,
                                    offset_end=align$offset_scan_last))
  #Get (last) unique
  echem_args<-echem_args[length(names(echem_args))-match(unique(names(echem_args)),rev(names(echem_args)))+1]
  #set margins of RH plot to leave no margin on LHS
  par(mai=c(margins[1],0.05,margins[3:4]))
  #Plot echem
  do.call(plot_echem_vertical,echem_args)

  layout(1)
  par(original_par)
}


#' Plot electrochemisty data (vertically)
#'
#' This function plots in-situ echem data.
#' @param data Echem data
#' @param V_range Voltage range of echem data to plot
#' @param time_start,time_end Range of time to plot
#' @param y_range Y range to scale the data to
#' @param offset_start,offset_end y values of 1st and last data point
#' @param show_axes --ignored--
#' @param V_tick_interval,V_minor_tick_interval,V_minor_tick_interval --ignored--
#' @param time_tick_interval,time_ticks_dps,tick_size_multiplier --ignored--
#' @param xaxismline Margin line on which to draw the x axis
#' @param xaxislabelmline Margin line on which to draw the x axis title
#' @export
plot_echem_vertical <- function(data,V_range=NA,time_start=0, time_end=Inf,
                                yrange=c(0,1),offset_start=0,offset_end=1,
                                show_axes=TRUE,V_tick_interval=NA,
                                V_minor_tick_interval=NA,time_tick_interval=NA,
                                time_ticks_dps=0,tick_size_multiplier=1,
                                xaxismline=-0.8,xaxislabelmline=1.1) {
  if(nrow(data) == 0) {
    data=data.frame(Test_Time.s. = NA, Voltage.V. = NA)
  } else {
    #restrict echem to region requested
    data=data[data$Test_Time.s. >= time_start * 3600 & data$Test_Time.s. <= time_end * 3600, ]
  }

  if(time_end==Inf) time_end=max(data$Test_Time.s.) else time_end=time_end*3600
  time_start=time_start*3600

  if(any(is.na(V_range))) V_range=c(min(data$Voltage.V.),max(data$Voltage.V.))

  total_y2_range = (time_end-time_start) / 3600 / ((offset_end - offset_start) / (yrange[[2]]-yrange[[1]]))
  first_scan_as_fraction_of_yrange = (offset_start-yrange[[1]]) / (yrange[[2]] - yrange[[1]])
  first_scan_in_y2 = first_scan_as_fraction_of_yrange * total_y2_range

  Plotting.Utils::pretty_plot(xlim=c(-V_range[[2]],-V_range[[1]]), ylim=yrange, axes=c(1,4),
                              y2lim=c(-first_scan_in_y2, total_y2_range-first_scan_in_y2),
                              frac=TRUE, div=c(2,5), invertX=TRUE,
                              upperTickLimit=c(NA,NA,time_end/3600),
                              lowerTickLimit=c(NA,NA,time_start/3600),
                              line=xaxismline,labline=xaxislabelmline,
                              xlab=expression("Voltage / V"), y2lab="Time / h")

  #plot echem
  lines(-data$Voltage.V.,data$Test_Time.s.*(offset_end-offset_start)/(time_end-time_start))
}
