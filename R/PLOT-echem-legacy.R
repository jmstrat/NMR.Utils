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
#' plot_echem_horizontal(data, V_range)
plot_echem_horizontal <- function(data, V_range, alignment=NA, show_axes=TRUE,
                                  V_tick_interval=0.25, time_tick_interval=5, time_ticks_dps=0, mar=2) {
  if (any(is.na(alignment))) {
    alignment <- list(xrange=c(0, 1), time_scan_1=0, offset_scan_1=0, time_scan_last=Inf, offset_scan_last=1)
  }
  # restrict echem to region of NMR data plotted
  data <- data[data$Test_Time.s. >= alignment$time_scan_1 * 3600 & data$Test_Time.s. <= alignment$time_scan_last * 3600, ]

  if (nrow(data) == 0) {
    data <- data.frame(Test_Time.s.=0, Voltage.V.=NA)
    alignment <- list(yrange=c(0, 1), time_scan_1=0, offset_scan_1=0, time_scan_last=Inf, offset_scan_last=1)
  }

  # blank plot
  plot(0, 0, type="n", xaxs="i", yaxs="i",
       xlim=alignment$xrange,
       ylim=c(V_range[[1]], V_range[[2]]), axes=FALSE, xlab=NA, ylab=NA)

  # plot echem
  lines(data$Test_Time.s. * (alignment$offset_scan_last - alignment$offset_scan_1) / (max(data$Test_Time.s.) - min(data$Test_Time.s.)),
        data$Voltage.V.)

  time_ticks_at <- seq(min(data$Test_Time.s.), max(data$Test_Time.s.), time_tick_interval * 3600)
  ScaledtimeTicksAt <- time_ticks_at * (alignment$offset_scan_last - alignment$offset_scan_1) / (max(data$Test_Time.s.) - min(data$Test_Time.s.))

  V_ticks_at <- seq(round(V_range[[1]], 2), round(V_range[[2]], 2), V_tick_interval)

  ## Add a box
  box()
  if (show_axes) {
    # Add axis with no labels
    axis(side=1, tck=-.015, at=ScaledtimeTicksAt, labels=NA)
    axis(side=mar, tck=-.015, at=V_ticks_at, labels=NA)

    ## Add axis With labels (With reduced spacing from axis -- line=.4)
    axis(side=1, lwd=0, at=ScaledtimeTicksAt, line=-1, labels=round(time_ticks_at / 3600, digits=time_ticks_dps), tck=-.015, cex.axis=1)
    axis(side=mar, lwd=0, at=V_ticks_at, line=-.4, las=1, labels=V_ticks_at)

    ## Add axis titles
    mtext(side=1, "Time (h)", line=1.8)
    mtext(side=mar, "Voltage (V)", line=2)
  }
}


xxx_plot_echem_vertical <- function(data, V_range=NA, Y_start=0, Y_end=Inf,
                                    yrange=c(0, 1), offset_start=0, offset_end=1,
                                    xaxismline=-0.8, xaxislabelmline=1.1, axes=c(1, 4),
                                    yaxismline=-0.8, yaxislabelmline=1.1,
                                    upperTickLimit=Y_end,
                                    lowerTickLimit=Y_start,
                                    ticklabels=c(T, T, T), div=c(2, 5),
                                    forcedInterval=NA, ticksOut=c(T, T),
                                    forcePrint=FALSE, centreTimeTitle=T) {
  if (nrow(data) == 0) {
    data <- data.frame(x=NA, y=NA)
  } else {
    xlab <- jms.classes::ylab(data)
    y2lab <- jms.classes::xlab(data)
    # Used for unit conversions (s -> hr; Ah -> mAh etc.)
    xscale <- jms.classes::xscale(data)
    yscale <- jms.classes::yscale(data)

    data <- data.frame(x=data[, ycol(data)], y=data[, xcol(data)])

    # restrict echem to region requested
    data[, 1] <- data[, 1] * yscale
    data[, 2] <- data[, 2] * xscale
    data <- data[data[, 2] >= Y_start & data[, 2] <= Y_end, ]
  }

  if (Y_end == Inf) Y_end <- max(data[, 2])

  if (any(is.na(V_range))) V_range <- range(data[, 1])

  total_y2_range <- (Y_end - Y_start) / ((offset_end - offset_start) / (yrange[[2]] - yrange[[1]]))
  time_start_as_fraction_of_yrange <- (offset_start - yrange[[1]]) / (yrange[[2]] - yrange[[1]])
  time_zero_in_y2 <- time_start_as_fraction_of_yrange * total_y2_range - Y_start

  Plotting.Utils::pretty_plot(
    xlim=c(V_range[[2]], V_range[[1]]), ylim=yrange, axes=axes,
    y2lim=c(-time_zero_in_y2, total_y2_range - time_zero_in_y2),
    frac=TRUE, div=div,
    upperTickLimit=c(NA, NA, upperTickLimit),
    lowerTickLimit=c(NA, NA, lowerTickLimit),
    line=c(xaxismline, yaxismline, yaxismline),
    labline=c(xaxislabelmline, yaxislabelmline, yaxislabelmline),
    xlab=xlab, y2lab=y2lab, ticklabels=ticklabels,
    forcedInterval=forcedInterval, ticksOut=ticksOut,
    forcePrint=forcePrint, centreTitlesToLabels=c(F, F, centreTimeTitle)
  )

  # LEGACY -- these functions have changed
  # rescale <- echemRescaleFactory(offset_end, offset_start, Y_end, Y_start)
  # inverse <- inverseEchemRescaleFactory(offset_end, offset_start, Y_end, Y_start)

  # plot echem
  # lines(rescale(data))

  # Return rescale function
  # invisible(list(rescale, inverse))
}
