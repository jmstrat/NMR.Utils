#' Plot examples of fitted frames
#'
#' This function plots the deconvolution for specified frames from the fit.
#' @param fit The fit object
#' @param data The data object
#' @param scans Which scans to include in the plot
#' @param yrange Y range of data to plot (or 'auto')
#' @param xrange X range of data to plot (default is integration range)
#' @param plot_offset offset between subsequent plots
#' @param xTickInterval Tick interval for x axis
#' @param cols Colour for each deconvolution
#' @param fit_col Colour for the fit
#' @param data_col Colour for the data
#' @param lwd Line width for each deconvolution
#' @param lty Line type for each deconvolution
#' @param fit_lwd Line width for the fit
#' @param data_lwd Line width for the data
#' @param show_axes (TRUE / FALSE) Should plot axes & labels
#' @return None
#' @export
#' @examples
#' plot_examples(fit,data,scans=seq(1:20,5),plot_offset=100000)
plot_examples <-function(fit,data,scans,plot_offset,yrange='auto',xrange=NA,cols=c('forestgreen','blue','magenta','cyan','orange','brown'),fit_col='red', data_col='black',fit_lwd=2,lwd=1,data_lwd=1,lty='dashed',xTickInterval=50,show_axes=TRUE) {
  if(any(yrange=='auto')) {
    #calculate y-range for plot
    highest_scan=max(scans)
  
    y_max=data[,highest_scan+1]
    y_max[data[,1]>fit$integration_range[[2]]]<-0
    y_max[data[,1]<fit$integration_range[[1]]]<-0
    y_max=max(y_max)
    y_max=y_max+plot_offset*(length(scans)-1)
    y_max=y_max*1.02
  
    y_min=data[,scans[[1]]+1]
    y_min[data[,1]>fit$integration_range[[2]]]<-0
    y_min[data[,1]<fit$integration_range[[1]]]<-0
    y_min=min(y_min)
    y_min=y_min*1.02
    
    yrange=c(y_min,y_max)
  }
  if(any(is.na(xrange))) {
    xrange=c(fit$integration_range[[2]],fit$integration_range[[1]])
  }
  
  #plot (blank plot)
  plot(0,0, type="n", xaxs="i", yaxs="i", xlim=xrange, ylim=yrange, axes = FALSE, xlab = NA, ylab = NA)
  
  for(a in 1:length(scans)){
    off=(a-1)*plot_offset
    
    i=scans[[a]]
    
    #plot data
    lines(data[,1],data[,(i+1)]+off,col=data_col,lwd=data_lwd)
    
    #Get fitted models
    fit_models=make_fitted_models(fit$models,fit$result[i,])
    
    #Plot the deconvolution:
    
    for(f in fit_models) {
      curve(f[[2]](x)+off,from=xrange[[2]],to=xrange[[1]],lwd=lwd,lty=lty,col=cols[[f[[1]]]],add=T,n=length(data[,1]))
    }
    
    #Build a total fit model
    total_fit_function=get_fit_function(fit_models)
    
    #plot overall fit
    curve(total_fit_function(x)+off,from=xrange[[2]],to=xrange[[1]],col=fit_col,lwd=fit_lwd,add=T,n=length(data[,1]))
  }
  #make plot nice
  box()
  if(show_axes){
    ticksat=seq(xrange[[2]],xrange[[1]],xTickInterval*sign(xrange[[1]]-xrange[[2]]))
  
    #Add axis with no labels
    axis(side = 2, tck = -.015, tick=FALSE, labels=NA)
    axis(side = 1, tck = -.015, labels = NA,at=ticksat)
  
    ##Add axis With labels (With reduced spacing from axis -- line=.4)
    axis(side = 2, lwd = 0, line = -.4, tick=FALSE, labels=NA)
    axis(side = 1, lwd = 0, line = -.6, las = 1,at=ticksat,labels=ticksat)
  
    ##Add axis titles
    mtext(side = 2, "Intensity (a.u.)", line = 0.5)
    mtext(side = 1, "Shift (ppm)", line = 1.5)
    
    #Add labels
    axis(side = 4, tck = 0, tick=FALSE, labels=scans, at=0:(length(scans)-1)*plot_offset,line = -.8,las=1)
  }
}

#' Plot the integrated intensities
#'
#' This function plots the integrated intensities of the fit and the deconvolution for all frames in the fit.
#' @param fit The fit object
#' @param pch The character to use for points (3 = + , 4 = x , 16 = filled circle etc.) (see \code{\link[graphics]{points}})
#' @param fit_col Colour for the total integral
#' @param deconv_cols Colours for the deconvolution integrals
#' @param error_alpha Factor to change the alpha when plotting errors
#' @param plot_total_integral TRUE / FALSE should plot the total integral
#' @param mar side to show axis on
#' @param show_axes (TRUE / FALSE) Should plot axes & labels
#' @return None
#' @export
#' @examples
#' plot_integrals(fit)
plot_integrals <- function(fit,pch=4, fit_col='red',deconv_cols=c('forestgreen','blue','magenta','cyan','orange','brown'), error_alpha=0.5,plot_total_integral=TRUE, mar=2,show_axes=TRUE) {
  #plot (blank plot)
  plot(0,0, type="n", xaxs="i", yaxs="i", ylim=c(0,1),xlim=c(0,1), axes = FALSE, xlab = NA, ylab = NA)
  
  offsets=seq(0,1,length.out=nrow(fit$result))
  
  #plot areas
  
  total_integral=fit$result[,'fit_integrated_area']
  max_integral=max(total_integral)
  
  for(m in 1:length(fit$models)) {
    
	  integrals=fit$result[,paste0('integrated_area_',m)]
	  stderrs=fit$result[,paste0('std.error_',m)]*(fit$integration_range[[2]]-fit$integration_range[[1]])
    integrals[is.na(integrals)]<-0
    stderrs[is.na(stderrs)]<-0
    
	  int_min=integrals-stderrs
	  int_max=integrals+stderrs
    points(offsets,integrals/max_integral,pch=pch,col=deconv_cols[[m]])

    #errors
    col1=adjustcolor(deconv_cols[[m]], alpha.f=error_alpha)
    xpoints=c(offsets,rev(offsets))
    ypoints= c(int_max,rev(int_min))
    polygon(xpoints[!is.na(ypoints)],ypoints[!is.na(ypoints)]/max_integral,col=col1,border='NA')
  }
  
  if(plot_total_integral) {
    points(offsets,total_integral/max_integral,pch=pch,col=fit_col)
    
    col1=adjustcolor(fit_col, alpha.f=error_alpha)

    xpoints=c(offsets,rev(offsets))
    sde=fit$result[,'std.error']*(fit$integration_range[[2]]-fit$integration_range[[1]])

    int_min=total_integral-sde
    int_max=total_integral+sde

    ypoints= c(int_max,rev(int_min))
    polygon(xpoints[!is.na(ypoints)],ypoints[!is.na(ypoints)]/max_integral,col=col1,border='NA')
	}
  
  
  #if(plot_0_100) {
  #  lines(c(offsets[[1]],offsets[[length(offsets)]]),c(0,0),col='darkgrey')
  #  lines(c(offsets[[1]],offsets[[length(offsets)]]),c(1,1),col='darkgrey')
  #}
  #make plot nice
  box()
  
  if(show_axes) {
    #Add axis with no labels
    axis(side = mar, tck = -.015, labels=NA,at=seq(0,1,0.1))
  
    ##Add axis With labels (With reduced spacing from axis -- line=.4)
    axis(side = mar, lwd = 0, line = -0.6, labels=seq(0,100,20),at=seq(0,1,0.2))
  
    ##Add axis titles
    lab="Relative area (%)"
    mtext(side = mar, lab, line = 1.8)
  }
}

#' Plot a fitted parameter
#'
#' This function plots a parameter of the deconvolution for all frames in the fit.
#' @param fit The fit object
#' @param parameter_name The name of the parameter to plot
#' @param pch The character to use for points (3 = + , 4 = x , 16 = filled circle etc.) (see \code{\link[graphics]{points}})
#' @param cols Colours for the parameters
#' @param show_axes (TRUE / FALSE) Should plot axes & labels
#' @return None
#' @export
#' @examples
#' plot_parameter(fit,'height')
plot_parameter <- function(fit,parameter_name,pch=4,cols=c('forestgreen','blue','magenta','cyan','orange','brown'),show_axes=TRUE) {

  column_names=names(fit$result)
  column_names=column_names[grepl(paste0('^',parameter_name,'_[[:digit:]]*$'), column_names)]
  
  param_table=as.data.frame(fit$result[,column_names])
  
  max_value=max(param_table, na.rm=TRUE)
  min_value=min(param_table, na.rm=TRUE)
  plot(0,0, type="n", xaxs="i",ylim=c(min_value,max_value),xlim=c(0,1), axes = FALSE, xlab = NA, ylab = NA)
  
  offsets=seq(0,1,length.out=nrow(fit$result))
  
	for(i in 1:ncol(param_table)) {
    nam=column_names[[i]]
    m=strsplit(nam,"_")[[1]]
    m=as.numeric(m[[length(m)]])
    points(offsets,param_table[,i],pch=pch,col=cols[[m]])
  }
  
  #make plot nice
  box()
  if(show_axes) {
    #Add axis with no labels
    axis(side = 2, tck = -.015, labels=NA)

    ##Add axis With labels (With reduced spacing from axis -- line=.4)
    axis(side = 2, lwd = 0, line = -0.6)

    mtext(side = 2, parameter_name, line = 1.8)
  }
}

#' Produce some summary plots
#'
#' This function plots some summary plots to show the fit.
#' @param fit The fit object
#' @param data The data object
#' @param echem The echem object
#' @return None
#' @export
#' @examples
#' plot_fit(fit,data,echem)
plot_fit <-function(fit,data,echem) {
  
  #3 plots
  #2 adjacent horizontal plot width ratio 1:1
  #RHS split vertically
  layout(matrix(c(1,1,2,3), 2, 2, byrow=F), widths=c(1,1))
  
  #Store par settings
  original_par=par('mar','xpd')
  
  #set margins for LH plot
  par(mar=c(3,3,1,1),xpd=FALSE)
  plot_examples(fit,data,c(1,round(ncol(data)/2)-1,ncol(data)-1),max(data[,2])/2)
  
  #set margins for RH top plot
  par(mar=c(0.5,1,1,3))
  plot_integrals(fit,mar=4)
  
  #set margins for RH bottom plot
  par(mar=c(3,1,0.5,3))
  plot_echem_horizontal(echem,c(min(echem$Voltage.V),max(echem$Voltage.V)),mar=4)

  model_vars=c()
  for(m in fit$models) {
  	vars=names(m$constraint)
  	for(v in vars) {
  		if(!v %in% model_vars) {
  			model_vars=append(model_vars,v)
  		}
  	}
  }

  nPlots=length(model_vars)+2
  #plots vertically aligned
  layout(matrix(1:nPlots,nPlots,1))

  #set margins for Top plot
  par(mar=c(0.5,3,1,1),xpd=FALSE)
  plot_integrals(fit)

  #set margins for middle plots
  par(mar=c(0.5,3,0.5,1),xpd=FALSE)



  for(m in model_vars) {
    plot_parameter(fit,m)
  }

	#set margins for bottom plot
	par(mar=c(3,3,0.5,1),xpd=FALSE)
	plot_echem_horizontal(echem,c(min(echem$Voltage.V),max(echem$Voltage.V)))
  
  #Restore settings
  layout(1)
  par(original_par)
}