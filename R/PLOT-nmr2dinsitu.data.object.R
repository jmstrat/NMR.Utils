#' Plot In situ NMR data
#'
#' This function plots in situ NMR data.
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
#'
#' @details The echem function will always be passed the echem data restricted to the NMR scans plotted as its first argument,
#' and a ylim parameter which will align its data as another argument. Any further named arguments provided via \code{...}
#' are passed through. \cr\cr
#' The NMR function will always be passed the NMR data as its first argument. Any further named arguments provided via \code{...}
#' are passed through. It is expected to return a numeric vector of the y values corresponding to an intensity of 0 for the
#' first and last scans.
#' @return Returns a function that will change the active plot to the NMR plot without disrupting the current layout
#' (the echem plot is active by default when the plot function returns).
#'
#' @export
#' @examples
#' plot(data)
#'
#' set_nmr <- plot(data)
#'
#' # Add some annotations to the echem plot
#' time <- c(1, 2)
#' V <- Plotting.Utils::nearest(echem, Test_Time.s.=time * 3600)$Voltage.V
#' points(V, time, col=c("red", "blue"))
#'
#' # Activate the nmr plot
#' set_nmr()
#' # Add annotations to the NMR plot
#' points(0, 10000, col="green")
plot.nmr2dinsitu.data.object <- function(nmr, use.default.layout=TRUE, separation=0.1,
                                         separation.pos=c(4, 2), nmr.func=NULL, echem.func=NULL, ...) {
  if (!requireNamespace("Echem.Data", quietly=TRUE)) {
    stop("Insitu data processing requires the Echem.Data package")
  }

  jms.classes::log.debug("Plotting an insitu data object")

  if (is.null(nmr.func)) nmr.func <- getS3method("plot", "nmr2d.data.object")
  if (is.null(echem.func)) echem.func <- plot_echem_vertical

  echem <- attr(nmr, "echem")
  if (!Echem.Data::is.echem.data.object(echem)) stop("Echem data is invalid")

  args <- list(...)
  echem_args <- args[names(args) %in% names(formals(echem.func))]
  NMR_args <- args[names(args) %in% names(formals(nmr.func))]

  margins <- par("mai")

  # Adjacent horizontal plot width ratio 3:1
  if (use.default.layout) layout(matrix(c(1, 2), 1, 2), widths=c(3, 1))

  set_margins <- function(n) {
    if (!(is.null(separation) || is.na(separation))) {
      pos <- separation.pos[[n]]
      if (pos == 1) {
        mai <- c(separation / 2, margins[2:4])
      } else if (pos == 4) {
        mai <- c(margins[1:3], separation / 2)
      } else {
        mai <- c(margins[1:(pos - 1)], separation / 2, margins[(pos + 1):4])
      }
      par(mai=mai)
    }
  }

  # Set margins for NMR plot (#1)
  set_margins(1)

  NMR_args <- append(list(nmr), NMR_args)
  # Plot NMR data and recieve time and offset of first and last scan plotted
  align <- do.call(nmr.func, NMR_args)

  coordinates <- list(
    usr=par("usr"),
    plt=par("plt"),
    fig=par("fig")
  )

  set_nmr <- SetNMRPlotFactory(coordinates)

  # Alignment
  jms.classes::log.debug("Attempting to align echem plot")
  echem_scan_points <- as.numeric(colnames(nmr)[-1])
  # Range of echem data to plot
  Y_start <- echem_scan_points[[1]]
  Y_end <- echem_scan_points[[length(echem_scan_points)]]
  # Y range of NMR Plot
  yrange <- par("usr")[3:4]
  # Y_start and Y_end in nmr units
  offset_start <- align[[1]]
  offset_end <- align[[2]]

  if (offset_end <= offset_start) {
    aligned_ylim <- NULL
    jms.classes::log.warn("Could not align echem plot")
    warning("Could not align NMR and echem data", call.=FALSE)
  } else {
    total_y_range <- (Y_end - Y_start) / ((offset_end - offset_start) / (yrange[[2]] - yrange[[1]]))
    time_start_as_fraction_of_yrange <- (offset_start - yrange[[1]]) / (yrange[[2]] - yrange[[1]])
    time_zero_in_y <- time_start_as_fraction_of_yrange * total_y_range - Y_start

    # These are the axis limits for the plot in the units of the echem data
    aligned_ylim <- c(-time_zero_in_y, total_y_range - time_zero_in_y)
  }

  jms.classes::log.debug("Reducing echem data to match NMR scans plotted")
  # Restrict echem to region requested
  echem <- echem[echem[, xcol(echem)] >= Y_start / xscale(echem) & echem[, xcol(echem)] <= Y_end / xscale(echem), ]

  jms.classes::log.debug("Reduced echem range to: [%s] (%s points)", paste0(range(echem[, xcol(echem)]), collapse=", "), nrow(echem))

  echem_args <- append(list(echem), echem_args)
  # Get (last) unique
  echem_args <- echem_args[length(names(echem_args)) - match(unique(names(echem_args)), rev(names(echem_args))) + 1]

  # Add ylim
  echem_args$ylim <- aligned_ylim

  # Set margins for Echem plot (#2)
  set_margins(2)

  # Plot echem
  do.call(echem.func, echem_args)

  jms.classes::log.debug("Finished plotting")

  invisible(set_nmr)
  # Note we cannot restore par / layout as that would affect later plots
}

# Break this out into a separate function so we don't capture the data in the environment
SetNMRPlotFactory <- function(coordinates) {
  jms.classes::log.debug("Preparing function to activate NMR plot")
  # Note this won't work properly for overlapping plots
  function() {
    # We cannot set par(fig) without disrupting the layout. Even if we pass new=TRUE
    # it will only work if we have drawn every plot already.
    # So we need to map the nmr usr coordinates onto the current plot; then set clip
    # This assumes we're not using logarithmic coordinates

    # NMR plot Bounds in ndc
    nmr_ndc <- c(
      coordinates$plt[1:2] * (coordinates$fig[[2]] - coordinates$fig[[1]]) + coordinates$fig[[1]],
      coordinates$plt[3:4] * (coordinates$fig[[4]] - coordinates$fig[[3]]) + coordinates$fig[[3]]
    )

    # Current plot Bounds in ndc
    current_ndc <- c(
      grconvertX(c(0, 1), from="npc", to="ndc"),
      grconvertY(c(0, 1), from="npc", to="ndc")
    )

    # Now we need usr values for the current plot as if it were an extension to the NMR plot.
    # This will mean that user coordinates are correct for the NMR plot.
    usr <- c(
      coordinates$usr[[1]] - (coordinates$usr[[1]] - coordinates$usr[[2]]) / (nmr_ndc[[1]] - nmr_ndc[[2]]) * (nmr_ndc[[1]] - current_ndc[[1]]),
      coordinates$usr[[2]] - (coordinates$usr[[1]] - coordinates$usr[[2]]) / (nmr_ndc[[1]] - nmr_ndc[[2]]) * (nmr_ndc[[2]] - current_ndc[[2]]),

      coordinates$usr[[3]] - (coordinates$usr[[3]] - coordinates$usr[[4]]) / (nmr_ndc[[3]] - nmr_ndc[[4]]) * (nmr_ndc[[3]] - current_ndc[[3]]),
      coordinates$usr[[4]] - (coordinates$usr[[3]] - coordinates$usr[[4]]) / (nmr_ndc[[3]] - nmr_ndc[[4]]) * (nmr_ndc[[4]] - current_ndc[[4]])
    )

    par(usr=usr)

    # Set the clipping region to cover the NMR plot
    do.call(clip, as.list(coordinates$usr))

    # Return the NMR coordinates invisibly
    invisible(coordinates)
  }
}

#' Plot electrochemisty data (vertically)
#'
#' This function plots in-situ echem data. It is effectively the same as plot,
#' but swaps the x and y axes, and renames some arguments to avoid clashes with
#' \code{\link{plot.nmr2d.data.object}}.
#' @param data Echem data
#' @param V_range X range of echem data to plot
#' @param echemAxes Axes for the echem plot
#' @param xaxismline,yaxismline Margin line on which to draw the axes
#' @param xaxislabelmline,yaxislabelmline Margin line on which to draw the axis titles
#' @param upperVTickLimit,lowerVTickLimit Voltage tick limits
#' @param forcedVInterval Forced voltage tick interval
#' @param echemDiv Number of minor ticks per major c(voltage, y)
#' @param centreTimeTitle Centre the y axis label to the tick marks?
#' @inheritParams Plotting.Utils::pretty_axes
#' @export
plot_echem_vertical <- function(
                                data, V_range=NULL, ylim=NULL,
                                echemAxes=c(1, 4), echemDiv=c(2, 5),
                                xaxismline=-0.8, xaxislabelmline=1.1,
                                yaxismline=-0.8, yaxislabelmline=1.1,
                                upperVTickLimit=NA,
                                lowerVTickLimit=NA,
                                ticklabels=c(T, T, T), ticksOut=c(T, T),
                                forcedVInterval=NA, forcePrint=FALSE,
                                centreTimeTitle=T, ...) {
  jms.classes::log.debug("Plotting an echem data object vertically")

  if (nrow(data) == 0 || ncol(data) == 0) {
    jms.classes::log.error("No data available: rows: %s, cols: %s", nrow(data), ncol(data))
    stop("No echem data to plot!")
  }

  if (length(xcol(data)) != 1 || is.na(xcol(data))) {
    jms.classes::log.error(
      "xcol is not correct: xcol = [%s]; ycol = [%s]",
      paste0(xcol(data), collapse=","),
      paste0(ycol(data), collapse=",")
    )
    stop("Invalid echem data")
  }

  if (length(ycol(data)) != 1 || is.na(ycol(data))) {
    jms.classes::log.error(
      "ycol is not correct: xcol = [%s]; ycol = [%s]",
      paste0(xcol(data), collapse=","),
      paste0(ycol(data), collapse=",")
    )
    stop("Invalid echem data")
  }

  # This is the range we will draw ticks for
  r <- range(data[, xcol(data)]) * xscale(data)
  # Add 1% to ensure we capture the ends
  r <- grDevices::extendrange(r=r, f=0.01)

  # Invert axes.
  y <- xcol(data)
  xcol(data) <- ycol(data)
  ycol(data) <- y

  y <- xscale(data)
  xscale(data) <- yscale(data)
  yscale(data) <- y

  if (4 %in% echemAxes) {
    # Axis on RHS
    y2lab(data) <- xlab(data)
    xlab(data) <- ylab(data)
    ylab(data) <- NA
  } else {
    # Axis on LHS
    y <- xlab(data)
    xlab(data) <- ylab(data)
    ylab(data) <- y
    y2lab(data) <- NA
  }

  if (is.null(V_range)) {
    V_range <- range(data[, xcol(data)])
  }

  plot(
    data,
    xlim=rev(V_range),
    ylim=ylim,
    axes=echemAxes,
    frac=c(T, F), # Voltage can have a fractional tick interval
    div=echemDiv,
    upperTickLimit=c(upperVTickLimit, r[[2]]), # Only draw ticks where we have data
    lowerTickLimit=c(lowerVTickLimit, r[[1]]), # Only draw ticks where we have data
    line=c(xaxismline, yaxismline),
    labline=c(xaxislabelmline, yaxislabelmline),
    forcedInterval=c(forcedVInterval, NA),
    ticksOut=ticksOut,
    forcePrint=forcePrint,
    centreTitlesToLabels=c(F, centreTimeTitle),
    ...
  )
}
