#' Plot electrochemisty data (horizontally)
#'
#' This function plots in-situ echem data.
#' @param data Data frame of echem data
#' @param V_range Range of potential (Voltage) to plot
#' @param alignment Alignment parameters
#' @param show_axes (TRUE / FALSE) Should plot axes & labels
#' @param V_tick_interval Tick interval for the potential axis
#' @param time_tick_interval Tick interval for the time axis
#' @export
#' @examples
#' plot_echem_horizontal(data,V_range)
plot_echem_horizontal <-function(data,V_range, alignment=NA,show_axes=TRUE,V_tick_interval=0.25,time_tick_interval=5,time_ticks_dps=0,mar=2) {
  if(any(is.na(alignment))) {
    alignment=list(xrange=c(0,1),time_scan_1=0,offset_scan_1=0,time_scan_last=Inf,offset_scan_last=1)
  }
  #restrict echem to region of NMR data plotted
  data=data[data$Test_Time.s.>=alignment$time_scan_1*3600& data$Test_Time.s.<=alignment$time_scan_last*3600,]

  if(nrow(data)==0) {
    data=dummy.echem()
    alignment=list(yrange=c(0,1),time_scan_1=0,offset_scan_1=0,time_scan_last=Inf,offset_scan_last=1)
  }

  #blank plot
  plot(0,0, type="n", xaxs="i", yaxs="i", xlim=alignment$xrange, ylim=c(V_range[[1]],V_range[[2]]), axes = FALSE, xlab = NA, ylab = NA)

  #plot echem
  lines(data$Test_Time.s.*(alignment$offset_scan_last-alignment$offset_scan_1)/(max(data$Test_Time.s.)-min(data$Test_Time.s.)),data$Voltage.V.)

  time_ticks_at=seq(min(data$Test_Time.s.),max(data$Test_Time.s.),time_tick_interval*3600)
  ScaledtimeTicksAt=time_ticks_at*(alignment$offset_scan_last-alignment$offset_scan_1)/(max(data$Test_Time.s.)-min(data$Test_Time.s.))

  V_ticks_at=seq(round(V_range[[1]],2),round(V_range[[2]],2),V_tick_interval)

  ##Add a box
  box()
  if(show_axes) {
      #Add axis with no labels
      axis(side = 1, tck = -.015,at=ScaledtimeTicksAt, labels = NA)
      axis(side = mar, tck = -.015,at=V_ticks_at, labels = NA)

      ##Add axis With labels (With reduced spacing from axis -- line=.4)
      axis(side = 1, lwd = 0,at=ScaledtimeTicksAt, line = -1,labels=round(time_ticks_at/3600,digits=time_ticks_dps),tck = -.015,cex.axis=1)
      axis(side = mar, lwd = 0,at=V_ticks_at, line = -.4, las = 1, labels=V_ticks_at)

      ##Add axis titles
      mtext(side = 1, "Time (h)", line = 1.8)
      mtext(side = mar, "Voltage (V)", line = 2)
  }
}
