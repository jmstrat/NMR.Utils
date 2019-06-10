#' Plot a raw frame for a fit animation
#'
#' This function plots a single frame from the fit.
#' @param fit The fit object
#' @param data The insitu data object
#' @param frame_number Which frame to plot
#' @param xlim x range of data to plot
#' @param ylim Y range of data to plot
#' @param cols Colour for each deconvolution
#' @param fit_col Colour for the fit
#' @param data_col Colour for the data
#' @param lwd Line width for each deconvolution
#' @param lty Line type for each deconvolution
#' @param fit_lwd Line width for the fit
#' @param data_lwd Line width for the data
#' @param ... Additional parameters passed to the NMR plotting function
#' @return None
#' @examples
#' plot_animation_frame(data, 100, fit)
#' @keywords internal
plot_animation_frame <-function(data, frame_number, fit=NULL, xlim=NULL, V_range=NULL, ylim=NULL,
                                data_col='black', data_lwd=1,
                                cols=c('forestgreen','blue','magenta','cyan','orange','brown'), lwd=1, lty='dashed',
                                fit_col='red', fit_lwd=2, ...) {

  echem = attr(data, 'echem')

  if(!is.null(echem)) {
    #2 adjacent horizontal plot width ratio 3:1
    layout(matrix(c(1, 2), 1, 2, byrow=F), widths=c(3,1))
  }

  #set margins for LH plot
  par(mai=c(0.5, 0.2, 0.05, 0.2),xpd=FALSE)

  #plot (blank plot)
  plot(data[,c(1,frame_number+1)], xlim=xlim, ylim=ylim, col=data_col, lwd=data_lwd, line=-0.6, labline=c(1.7, 1.6), ...)
  #plot data
  if(!is.null(fit)) {
    #Get fitted models
    fit_models=make_fitted_models(fit$models,fit$result[frame_number,])

    x=data[,1] #ppm
    #Plot the deconvolution:
    for(f in fit_models) {
      curve(f[[2]](x),from=xlim[[1]],to=xlim[[2]],lwd=lwd,lty=lty,col=cols[[f[[1]]]],add=T,n=length(data[,1]))
    }

    #Build a total fit model
    total_fit_function=get_fit_function(fit_models)

    #plot overall fit
    curve(total_fit_function(x),from=xlim[[1]],to=xlim[[2]],col=fit_col,lwd=fit_lwd,add=T,n=length(data[,1]))
  }

  if(!is.null(echem)) {
    #set margins of RH plot
	  par(mai=c(0.5, 0, 0.05, 0.6))
    plot_echem_vertical(echem, V_range=V_range, col=data_col, lwd=data_lwd, xaxismline=-0.6, xaxislabelmline=1.7, yaxismline=-0.2, yaxislabelmline=2)

    #add point
    time_scan = as.numeric(names(data)[[frame_number+1]]) / jms.classes::xscale(echem)

    args = list(echem)
    args[[colnames(echem)[[jms.classes::xcol(echem)]]]] = time_scan
    v = as.numeric(do.call(Plotting.Utils::nearest, args)[,jms.classes::ycol(echem)])
    points(v * jms.classes::yscale(echem), time_scan * jms.classes::xscale(echem),col='red',pch=16,cex=1)
  }
}

#' Make an animation
#'
#' This function makes an animation for the fit.
#' @inheritParams plot_animation_frame
#' @param delay Delay between frames in the animation
#' @param width Width of the animation
#' @param height Height of the animation
#' @param type Type of animation (gif or HTML) gif requires additional software (see \code{\link[animation]{saveGIF}})
#' @param name File name to output
#' @param image_dev Image device to use (if you get error messages about png, try setting this to 'jpeg')
#' @param image_type Image type to use (if you get error messages about png, try setting this to 'jpg')
#' @return None
#' @export
#' @examples
#' save_animation(data, fit)
save_animation <-function(data, fit=NULL, xlim=NULL, V_range=NULL, ylim=NULL,
                          data_col='black', data_lwd=1,
                          cols=c('forestgreen','blue','magenta','cyan','orange','brown'), lwd=1, lty='dashed',
                          fit_col='red', fit_lwd=2,
                          delay=0.1, width=600, height=400, type='gif',
                          name='Animation.gif', image_dev='png', image_type='png', ...) {
  #requires ImageMagick to also be installed
  animation::ani.options(interval=delay,ani.width=width,ani.height=height)
  if(type=="HTML"){
      func=animation::saveHTML
  } else if(type=="gif") {
      func=animation::saveGIF
  }
suppressMessages(
  func({
    cat("Making Plots for Animation:\n")
    nc=ncol(data)
    progress=time_remaining_start(nc-1)

    for (i in 1:(nc-1)) {
      plot_animation_frame(data, i, fit=fit, xlim=xlim, V_range=V_range, ylim=ylim,
                           data_col=data_col, data_lwd=data_lwd,
                           cols=cols, lwd=lwd, lty=lty,
                           fit_col=fit_col, fit_lwd=fit_lwd, ...)
      progress(i)
    }
    cat("\nSaving Animation\n")
  },movie.name = name, navigator=FALSE, ani.dev=image_dev, ani.type=image_type))
  cat(paste0("Animation saved at: ",normalizePath(dirname(name)),"/",name))
}
