#' Plot 2D NMR data
#'
#' This function plots 2D NMR Data.
#' @param nmrdata Data frame of (real) NMR data (ppm, intensity1, ...)
#' @param xrange x range of data to plot (ppm) -- should be in form c(upper, lower) or spectrum will be reversed
#' @param yrange Y range of data to plot (or 'auto')
#' @param plot_offset offset between subsequent plots
#' @param xTickInterval Tick interval for x axis
#' @param xMinorTickInterval Minor tick interval for x axis
#' @param y_trunc Truncate y values that are higher than this
#' @param col Line colour for plot
#' @param lwd Line width for plot
#' @param shade_under TRUE / FALSE shade under the plot
#' @param shade_col colour to shade plot
#' @param y_trunc_x_points X-ranges to draw truncation lines over
#' @param y_trunc_amp_div Factor to divide amplitude of truncation line
#' @param y_trunc_label_offset_factor Factor to offset truncation line label by
#' @param y_trunc_sin_period Period of sine wave for truncation line
#' @param y_trunc_labels List of labels for truncation line
#' @param y_trunc_text_col Colour of text for truncation line
#' @param y_trunc_line_col Colour of truncation line
#' @param y_trunc_lwd Line width of truncation line
#' @param show_axes (TRUE / FALSE) Should plot axes & labels
#' @param plot.colour TRUE
#' @param plot.colour.ranges list of x ranges to colour (can be NA to cover entire range)
#' @param plot.colour.yranges list of y ranges to scale colours over (can be 'auto')
#' @param show_RH_Tick (TRUE / FALSE) Show last tick on RHS
#' @return List of alignment_parameters
#' @export
#' @examples
#' plotNMR2D(data, xrange=c(1500,-500), yrange='auto', plot_offset=100000, plot.colour.ranges=list(c(210,900)), plot.colour.yranges=list(c(50000,200000)))
plotNMR2D <-function(nmrdata,xrange,yrange,plot_offset,plot.colour=TRUE,plot.colour.ranges=NA,plot.colour.yranges='auto', xTickInterval=200,xMinorTickInterval=50,y_trunc=Inf,col='Black',lwd=1,shade_under=FALSE,shade_col='grey',y_trunc_x_points=c(),y_trunc_amp_div=200,y_trunc_label_offset_factor=20,y_trunc_sin_period=5,y_trunc_labels=c(),y_trunc_text_col='grey',y_trunc_line_col='grey',y_trunc_lwd=2,show_axes=TRUE,show_RH_Tick=TRUE) {
  load_or_install("plotrix")
    n=ncol(nmrdata) 
    x=nmrdata[,1] #ppm
    offsets=as.numeric(names(nmrdata)[2:length(nmrdata)])
    #convert offsets in hours to plot coordinates
    pos=offsets*plot_offset

    if(any(yrange=='auto'))
    {
      yrangemin=min(nmrdata[,2]+pos[[1]])
      yrangemax=max(nmrdata[,n]+pos[[n-1]])
      yrange<-c(yrangemin,yrangemax)
    }
    #plot (blank plot)
    plot(0,0, type="n", xaxs="i", yaxs="i", xlim=xrange, ylim=yrange, axes = FALSE, xlab = NA, ylab = NA)

    if(plot.colour) {
      if(any(is.na(plot.colour.ranges))) {
        plot.colour.ranges=xrange
      }
      col_ranges=plot.colour.yranges
      if(any(col_ranges=='auto')) {
        for(r in plot.colour.ranges) {
          crange=c(Inf,-Inf)
          for(i in n:2) {
            cy=nmrdata[,i][x>r[[1]]&x<r[[2]]]
            mincy=min(cy)
            maxcy=max(cy)
            if(mincy<crange[[1]]) {
              if(mincy<plot.colour.min.intensity) {
                mincy=plot.colour.min.intensity
              }
              crange[[1]]=signif(mincy,5)
            }
            if(maxcy>crange[[2]]) {
              crange[[2]]=signif(maxcy,5)
            }
          }
          col_ranges=append(col_ranges,list(crange))
        }
      }
    }

    #loop over scans to plot (reverse order)
    for(i in n:2) {
      #calculate offset for scan
      off=pos[[(i-1)]]
      #add offset to intensity
      y=nmrdata[,i]+off
      #truncate points (add 20% to remove artefacts...)
      #y[y>y_trunc+y_trunc/5] <- NA
      #plot scan
      if(plot.colour) {
        allx=x
        ally=y
        for(r in 1:length(plot.colour.ranges)) {
          prx=plot.colour.ranges[[r]]
          pry=y[x>prx[[1]]&x<prx[[2]]]
          prx=x[x>prx[[1]]&x<prx[[2]]]
      
          ally[allx %in% prx] <- NA

          cr=col_ranges[[r]]
      
          cvar=pry-off
          cvar=signif(cvar,5)
          cvar[cvar<cr[[1]]]<-cr[[1]]
          cvar[cvar>cr[[2]]]<-cr[[2]]
          
          cols=color.scale.nmr(cvar,c(0,0,0,1,1,1),c(0,0,1,1,0,0),c(0,1,0,0,1,0),xrange=cr)
          nseg=length(prx)-1
          segments(prx[1:nseg], pry[1:nseg], prx[2:(nseg + 1)], pry[2:(nseg + 1)], col = cols,lwd=lwd)
          #tryCatch(color.scale.lines(prx,pry,c(0,0,0,1,1,1),c(0,0,1,1,0,0),c(0,1,0,0,1,0),colvar=cvar,lwd=lwd), error = function(e) lines(x,y,col=col,lwd=plot_line_lwd))
        }
        nseg=length(allx)
        segments(allx[1:nseg], ally[1:nseg], allx[2:(nseg + 1)], ally[2:(nseg + 1)], col = col,lwd=lwd)
        #lines(allx,ally,col=col,lwd=lwd)
      } else {
        lines(x,y,col=col,lwd=lwd)
      }
      if(shade_under){
        #add shading
        polygon(c(x,x[length(x)]), c(y, y[1]), col=shade_col,border=NA) 
      }
    }

    #loop over truncation lines
    if(length(y_trunc_x_points)>0){
      for(i in 1:length(y_trunc_x_points)) {
    
        #calculate parameters for sine wave
        xs=y_trunc_x_points[[i]]
        xcent=mean(xs)
        ycent=y_trunc/y_trunc_amp_div+y_trunc+y_trunc/y_trunc_label_offset_factor
    
        #calculate parameters for white fill region
        xs2=c(xs[[1]],xs)
        xs2=append(xs2,xs[[length(xs)]])
        ys2=c(yrange[[2]],y_trunc,y_trunc,yrange[[2]])
        #fill with white
        polygon(xs2,ys2,col="white",border=NA)
    
        #draw truncation line
        curve(sin(x*pi/y_trunc_sin_period)*y_trunc/y_trunc_amp_div+y_trunc,from=xs[[1]],to=xs[[2]],col=y_trunc_line_col,add=T,lwd=y_trunc_lwd)
        #add label
        text(xcent,ycent,y_trunc_labels[i],col=y_trunc_text_col)
      }
    }
    #make plot pretty
    box()
    
    if(show_axes) {
        ticksat=seq(xrange[[1]],xrange[[2]],xTickInterval*sign(xrange[[2]]-xrange[[1]]))
        minorTicksat=seq(xrange[[1]],xrange[[2]],xMinorTickInterval*sign(xrange[[2]]-xrange[[1]]))

        #Add axis with no labels
        #axis(side = 2, tck = -.015, tick=FALSE, labels=NA)
        axis(side = 1, tck = -.015, labels = NA,at=ticksat)
        axis(side = 1, tck = -.005, labels = NA,at=minorTicksat)

        ##Add axis With labels (With reduced spacing from axis -- line=.4)
        #axis(side = 2, lwd = 0, line = -.4, tick=FALSE, labels=NA)
        lab = ticksat
        if(!show_RH_Tick)
          if(xrange[[2]]==lab[[length(lab)]])
            lab[[length(lab)]]=''
        axis(side = 1, lwd = 0, line = -.4, las = 1,at=ticksat,labels=lab)

        ##Add axis titles
        mtext(side = 2, "Intensity", line = 1)
        mtext(side = 1, "ppm", line = 1.8)
    }
    #return alignment parameters
    invisible(list(time_scan_1=offsets[[1]], offset_scan_1=pos[[1]],time_scan_last=offsets[[n-1]], offset_scan_last=pos[[n-1]], yrange=yrange))

}

#' Colour NMR Data
#'
#' This function is a workaround for an issue with \code{\link[plotrix]{color.scale}}.
#' @keywords internal
color.scale.nmr <- function (x, cs1 = c(0, 1), cs2 = c(0, 1), cs3 = c(0, 1), alpha = 1, extremes = NA, na.color = NA, xrange = NULL, color.spec = "rgb") {
  naxs <- is.na(x)
  if (!is.na(extremes[1])) {
    colmat <- col2rgb(extremes)
    cs1 <- colmat[1, ]/255
    cs2 <- colmat[2, ]/255
    cs3 <- colmat[3, ]/255
    color_spec <- "rgb"
  }
  maxcs1 <- ifelse(color.spec == "hcl", 360, 1)
  maxcs2 <- ifelse(color.spec == "hcl", 100, 1)
  maxcs3 <- ifelse(color.spec == "hcl", 100, 1)
  #ncolors <- length(x)
  if (is.null(xrange)) {
    xrange <- range(x, na.rm = TRUE)
    drop.extremes <- FALSE
  } else {
    if (xrange[1] > min(x, na.rm = TRUE) || xrange[2] < max(x, 
                                                            na.rm = TRUE)) 
      stop("An explicit range for x must include the range of x values.")
    x <- c(xrange, x)
    drop.extremes = TRUE
  }
  ncolors <- length(x) ##MOVED THIS TO HERE
  ncs1 <- length(cs1)
  if (ncs1 > 1) {
    cs1s <- rep(cs1[ncs1], ncolors)
    xstart <- xrange[1]
    xinc <- diff(xrange)/(ncs1 - 1)
    for (seg in 1:(ncs1 - 1)) {
      segindex <- which((x >= xstart) & (x <= (xstart + 
                                                 xinc)))
      cs1s[segindex] <- rescale.nmr(x[segindex], cs1[c(seg, 
                                                   seg + 1)])
      xstart <- xstart + xinc
    }
    if (min(cs1s, na.rm = TRUE) < 0 || max(cs1s, na.rm = TRUE) > 
        maxcs1) 
      cs1s <- rescale.nmr(cs1s, c(0, maxcs1))
  } else cs1s <- rep(cs1, ncolors)
  ncs2 <- length(cs2)
  if (ncs2 > 1) {
    cs2s <- rep(cs2[ncs2], ncolors)
    xstart <- xrange[1]
    xinc <- diff(xrange)/(ncs2 - 1)
    for (seg in 1:(ncs2 - 1)) {
      segindex <- which((x >= xstart) & (x <= (xstart + 
                                                 xinc)))
      cs2s[segindex] <- rescale.nmr(x[segindex], cs2[c(seg, 
                                                   seg + 1)])
      xstart <- xstart + xinc
    }
    if (min(cs2s, na.rm = TRUE) < 0 || max(cs2s, na.rm = TRUE) > 
        maxcs2) 
      cs2s <- rescale.nmr(cs2s, c(0, maxcs2))
  } else cs2s <- rep(cs2, ncolors)
  ncs3 <- length(cs3)
  if (ncs3 > 1) {
    cs3s <- rep(cs3[ncs3], ncolors)
    xstart <- xrange[1]
    xinc <- diff(xrange)/(ncs3 - 1)
    for (seg in 1:(ncs3 - 1)) {
      segindex <- which((x >= xstart) & (x <= (xstart + 
                                                 xinc)))
      cs3s[segindex] <- rescale.nmr(x[segindex], cs3[c(seg, 
                                                   seg + 1)])
      xstart <- xstart + xinc
    }
    if (min(cs3s, na.rm = TRUE) < 0 || max(cs3s, na.rm = TRUE) > 
        maxcs3) 
      cs3s <- rescale.nmr(cs3s, c(0, maxcs3))
  } else cs3s <- rep(cs3, ncolors)
  if (drop.extremes) {
      cs1s <- cs1s[-(1:2)]
      cs2s <- cs2s[-(1:2)]
      cs3s <- cs3s[-(1:2)]
  }
  xdim <- dim(x)
  colors <- do.call(color.spec, list(cs1s, cs2s, cs3s, alpha = alpha))
  if (!is.null(xdim)) 
    colors <- matrix(colors, nrow = xdim[1])
  if (length(naxs)) 
    colors[naxs] <- na.color
  return(colors)
}

#' Rescale Data
#'
#' This function is a workaround for an issue with \code{\link[plotrix]{rescale}}.
#' @keywords internal
rescale.nmr <- function (x, newrange) 
{
  if (missing(x) | missing(newrange)) {
    usage.string <- paste("Usage: rescale(x,newrange)\n", "\twhere x is a numeric object and newrange is the new min and max\n", sep = "", collapse = "")
    stop(usage.string)
  }
  if (is.numeric(x) && is.numeric(newrange)) {
    xna <- is.na(x)
    if (all(xna)) 
      return(x)
    if (any(xna)) 
      xrange <- range(x[!xna])
    else xrange <- range(x)
    if (xrange[1] == xrange[2]) 
      return(rep_len(newrange[[1]],length(x)))
    mfac <- (newrange[2] - newrange[1])/(xrange[2] - xrange[1])
    return(newrange[1] + (x - xrange[1]) * mfac)
  } else {
    warning("Only numeric objects can be rescaled")
    return(x)
  }
}