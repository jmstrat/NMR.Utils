#' Plot a raw frame for a fit animation
#'
#' This function plots a single frame from the fit.
#' @param fit The fit object
#' @param data The data object
#' @param echem The echem object
#' @param frame_number Which frame to plot
#' @param xrange x range of data to plot
#' @param yrange Y range of data to plot (or 'auto')
#' @param include_fit include the deconvolution etc. in the plot
#' @param xTickInterval Tick interval for x axis
#' @param cols Colour for each deconvolution
#' @param fit_col Colour for the fit
#' @param data_col Colour for the data
#' @param lwd Line width for each deconvolution
#' @param lty Line type for each deconvolution
#' @param fit_lwd Line width for the fit
#' @param data_lwd Line width for the data
#' @return None
#' @examples
#' plot_animation_frame(fit,data,echem,100,c(200,-200),c(0,2))
#' @keywords internal
plot_animation_frame <-function(fit,data,echem,frame_number,xrange,V_range,yrange='auto',include_fit=TRUE,data_col='black',data_lwd=1,cols=c('forestgreen','blue','magenta','cyan','orange','brown'),lwd=1,lty='dashed',fit_col='red',fit_lwd=2,xTickInterval=50) {
  #2 adjacent horizontal plot width ratio 5:1
  layout(matrix(c(1, 2), 1, 2, byrow=F), widths=c(5,1))
  #set margins for LH plot to leave no margin on RHS
  par(mar=c(3,3,1,0),xpd=FALSE)

  x=data[,1] #ppm
  y=data[,frame_number+1]
  #plot (blank plot)
  if(any(yrange=='auto')) {
  	yr=c(min(y),max(y))
  } else {
	  yr=yrange
  }
  plot(0,0, type="n", xaxs="i", yaxs="i", xlim=xrange, ylim=yr, axes = FALSE, xlab = NA, ylab = NA)
  #plot data
  lines(x,y,col=data_col,lwd=data_lwd)
  if(include_fit) {
    #Get fitted models
    fit_models=make_fitted_models(fit$models,fit$result[frame_number,])

    #Plot the deconvolution:
    for(f in fit_models) {
      curve(f[[2]](x),from=xrange[[1]],to=xrange[[2]],lwd=lwd,lty=lty,col=cols[[f[[1]]]],add=T,n=length(data[,1]))
    }

    #Build a total fit model
    total_fit_function=get_fit_function(fit_models)

    #plot overall fit
    curve(total_fit_function(x),from=xrange[[1]],to=xrange[[2]],col=fit_col,lwd=fit_lwd,add=T,n=length(data[,1]))
  }

  #make plot pretty
  box()
  ticksat=seq(xrange[[1]],xrange[[2]],xTickInterval*sign(xrange[[2]]-xrange[[1]]))
  #Add axis with no labels
  axis(side = 2, tck = -.015, tick=FALSE, labels=NA)
  axis(side = 1, tck = -.015, labels = NA,at=ticksat)
  ##Add axis With labels (With reduced spacing from axis -- line=.4)
  axis(side = 2, lwd = 0, line = -.4, tick=FALSE, labels=NA)
  axis(side = 1, lwd = 0, line = -.4, las = 1,at=ticksat,labels=ticksat)
  ##Add axis titles
  mtext(side = 2, "Intensity", line = 1)
  mtext(side = 1, "ppm", line = 2)

	#set margins of RH plot to leave no margin on LHS
	par(mar=c(3,0,1,3))
  plot_echem_vertical(echem,V_range)

  #add point
  time_scan=as.numeric(names(data)[[frame_number+1]])*3600
  index=which(abs(echem$Test_Time.s.-time_scan)==min(abs(echem$Test_Time.s.-time_scan)))[[1]]
  points(x=-echem$Voltage.V[[index]],y=time_scan*1/(max(echem$Test_Time.s.)-min(echem$Test_Time.s.)),col='red',pch=16,cex=1)

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
#' save_animation(fit,data,echem,c(200,-200),c(0,2))
save_animation <-function(fit,data,echem,xrange,V_range,yrange='auto',include_fit=TRUE,data_col='black',data_lwd=1,cols=c('forestgreen','blue','magenta','cyan','orange','brown'),lwd=1,lty='dashed',fit_col='red',fit_lwd=2,xTickInterval=50, delay=0.1,width=400, height=400,type='gif',name='Animation.gif',image_dev='png',image_type='png') {
  #requires ImageMagick to also be installed
  animation::ani.options(interval=delay,ani.width=width,ani.height=height)
  if(type=="HTML"){
      func=animation::saveHTML
  } else if(type=="gif") {
      func=animation::saveGIF
  }
suppressMessages(
  func({
    cat("Making Plots for Animation:")
    nc=ncol(data)
    progress=time_remaining_start(nc-1)

    for (i in 1:(nc-1)) {
      plot_animation_frame(fit,data,echem,i,xrange,V_range,yrange,include_fit,data_col,data_lwd,cols,lwd,lty,fit_col,fit_lwd,xTickInterval)
      progress(i)
    }

  },movie.name = name,navigator=FALSE,ani.dev=image_dev,ani.type=image_type))
  cat(paste0("Animation saved at: ",normalizePath(dirname(name)),"/",name))
}
