#' Plot the integrated intensities
#'
#' This function plots the integrated intensities of the fit and the deconvolution for all frames in the fit.
#' @param fit The fit object
#' @param showErrors Should the errors be displayed (as shaded regions) (Default TRUE)
#' @param NAasZero Display Models not included in the fit with 0 area and 0 error? (Default FALSE)
#' @param showLegend Show a legend above the plot? (Default TRUE)
#' @param pch The character to use for points (3 = + , 4 = x , 16 = filled circle etc.) (see \code{\link[graphics]{points}})
#' @param fit_col Colour for the total integral
#' @param deconv_cols Colours for the deconvolution integrals
#' @param error_alpha Factor to change the alpha when plotting errors
#' @param plot_total_integral TRUE / FALSE should plot the total integral
#' @return None
#' @export
#' @examples
#' plot(fit)
plot.nmr.fit.object <- function(fit,showErrors=TRUE, NAasZero=FALSE, showLegend=TRUE, pch=4, fit_col='red',deconv_cols=c('forestgreen','blue','magenta','cyan','orange','brown'), error_alpha=0.3,plot_total_integral=TRUE) {
  if(all(is.na(fit$result))) {
    stop('Fit must be run before plotting!')
  }
  if(all(is.na(fit$timeAxis))) {
    Plotting.Utils::pretty_plot(c(0,1),c(0,100),ylab = "Relative area (%)", xlab = 'Time', axes = 2)
    offsets=seq(0,1,length.out=nrow(fit$result))
  } else {
    Plotting.Utils::pretty_plot(range(fit$timeAxis),c(0,100),ylab = "Relative area (%)", xlab = 'Time / h', axes = c(1,2))
    offsets=fit$timeAxis
  }

  #plot areas

  total_integral=fit$result[,'fit_integrated_area']
  max_integral=max(total_integral)

  for(m in 1:length(fit$models)) {

    integrals=fit$result[,paste0('integrated_area_',m)]
    stderrs=fit$result[,paste0('std.error_',m)]*(fit$integration_range[[2]]-fit$integration_range[[1]])
    if(NAasZero) {
      integrals[is.na(integrals)]<-0
      stderrs[is.na(stderrs)]<-0
    }

    int_min=integrals-stderrs
    int_max=integrals+stderrs
    points(offsets,integrals/max_integral * 100,pch=pch,col=deconv_cols[[m]])

    #errors
    if(showErrors) {
      col1=adjustcolor(deconv_cols[[m]], alpha.f=error_alpha)
      xpoints=c(offsets,rev(offsets))
      ypoints= c(int_max,rev(int_min))
      polygon(xpoints[!is.na(ypoints)],ypoints[!is.na(ypoints)]/max_integral * 100,col=col1,border='NA')
    }
  }

  if(plot_total_integral) {
    points(offsets,total_integral/max_integral * 100,pch=pch,col=fit_col)
    if(showErrors) {
      col1=adjustcolor(fit_col, alpha.f=error_alpha)

      xpoints=c(offsets,rev(offsets))
      sde=fit$result[,'std.error']*(fit$integration_range[[2]]-fit$integration_range[[1]])

      int_min=total_integral-sde
      int_max=total_integral+sde

      ypoints= c(int_max,rev(int_min))
      polygon(xpoints[!is.na(ypoints)],ypoints[!is.na(ypoints)]/max_integral * 100,col=col1,border='NA')
    }
  }
  if(showLegend) {
    leg = sapply(fit$models, function(x) x$name)
    leg = c('Total', leg)
    cols = deconv_cols[1:length(fit$models)]
    cols = c(fit_col, cols)
    legend('top', horiz=T, inset=-0.1, xpd=NA, leg, col = cols, pch=pch)
  }
}
