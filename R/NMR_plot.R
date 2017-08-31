#' Plot 2D NMR data
#'
#' This function plots 2D NMR Data.
#' @param nmrdata Data frame of (real) NMR data (ppm, intensity1, ...)
#' @param xrange x range of data to plot (ppm) -- should be in form c(upper, lower) or spectrum will be reversed (or 'auto')
#' @param yrange Y range of data to plot (or 'auto')
#' @param plot_offset offset between subsequent plots (or 'auto')
#' @param xTickInterval Tick interval for x axis (or 'auto')
#' @param xMinorTickInterval Minor tick interval for x axis (or 'auto')
#' @param y_trunc Truncate y values that are higher than this (or NA for auto)
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
#' @param y_trunc_cex cex of truncation label
#' @param show_axes (TRUE / FALSE) Should plot axes & labels
#' @param plot.colour (TRUE / FALSE) Should the plot be coloured?
#' @param plot.colour.ranges list of x ranges to colour (can be NA to cover entire range)
#' @param plot.colour.yranges list of y ranges to scale colours over (can be 'auto')
#' @param show_RH_Tick (TRUE / FALSE) Show last tick on RHS
#' @param show_LH_Tick (TRUE / FALSE) Show last tick on LHS
#' @param xaxismline The margin line on which to draw the x-axis
#' @param xaxislabelmline The margin line on which to draw the x-axis label
#' @param colour_scheme If plot.colour is TRUE, each colour y-range (as specified in plot.colour.yranges) will be split into a number of equal ranges based on the number of colours specified in this vector. Each range will be assigned to one of the colours, starting from the smallest y-value.
#' @param col_na Colour of values outside of colouring y-range
#' @param colour.legend (TRUE / FALSE) Show a basic legend of the chosen colour_scheme
#' @param colour.legend.show.zero (TRUE / FALSE) If the colour range extends beyond zero, indicate this value in the legend (WARNING: Only valid if there is a single colour scaling range)
#' @return List of alignment_parameters
#' @export
#' @examples
#' plot.nmr.2Ddata.object(data, xrange=c(1500,-500), yrange='auto', plot_offset=100000, plot.colour.ranges=list(c(210,900)), plot.colour.yranges=list(c(50000,200000)))
plot.nmr.2Ddata.object<-function(nmrdata,xrange='auto',yrange='auto',plot_offset='auto',plot.colour=TRUE,plot.colour.ranges=NA,plot.colour.yranges='auto', xTickInterval='auto',xMinorTickInterval='auto',y_trunc=NA,col='Black',lwd=1,shade_under=FALSE,shade_col='grey',y_trunc_x_points=c(),y_trunc_amp_div=200,y_trunc_label_offset_factor=20,y_trunc_sin_period=5,y_trunc_labels=c(),y_trunc_text_col='grey',y_trunc_line_col='grey',y_trunc_lwd=2,y_trunc_cex=1,show_axes=TRUE,show_RH_Tick=TRUE,show_LH_Tick=TRUE,xaxismline=-0.8,xaxislabelmline=1.1,col_na='black',colour_scheme=c('blue','green','yellow','magenta','red'),colour.legend=FALSE,colour.legend.show.zero=TRUE) {
  load_or_install("plotrix")
  load_or_install('Plotting.Utils')
  n=ncol(nmrdata)
  x=nmrdata[,1] #ppm
  offsets=as.numeric(names(nmrdata)[2:n])

  #Subsetting drops attributes, but we only care about the nucleus
  nucleus=attr(nmrdata,'nuc1')
  if(is.null(nucleus)) nucleus='Unknown Nucleus'

  #Remove scans where offset = NA
  nmrdata=nmrdata[,c(TRUE,!is.na(offsets))]
  offsets=offsets[!is.na(offsets)]
  n=ncol(nmrdata)

  if(xrange=='auto') {
    xrange=c(max(nmrdata[,1]),min(nmrdata[,1]))
  }

  if(xTickInterval=='auto') xTickInterval=tick_interval(xrange[[1]]-xrange[[2]])
  if(xMinorTickInterval=='auto') xMinorTickInterval=xTickInterval/4

  if(plot_offset=='auto') {
    plot_offset=max(nmrdata[,c(2:n)])/sqrt(ncol(nmrdata)-1)
  }
  #convert offsets in hours to plot coordinates
  pos=offsets*plot_offset

  if(any(yrange=='auto'))
  {
    yrangemin=min(nmrdata[,2]+pos[[1]])
    yrangemax=max(nmrdata[,n]+pos[[n-1]])
    yrange<-c(yrangemin,yrangemax)
  }

  if(is.na(y_trunc))
    y_trunc=15/16*yrange[[2]]

  if(colour.legend) {
    #Add 1/2 inch to RHS (Must do this before plotting as it resets the layout order)
    par(omi=c(0,0,0,0.5))
  }
  #plot (blank plot)
  new_plot(xrange,yrange)

  if(plot.colour) {
    #Create colour lists
    if(all(is.na(colour_scheme))) {
      plot.colour=FALSE
      warning("colour_scheme is empty, but plot.colour is TRUE; setting plot.colour to FALSE and continuing", call.=FALSE)
    } else {
      colour_scheme=colour_scheme[!is.na(colour_scheme)]
      cols=col2rgb(colour_scheme)/255
      col_r=cols[1,]
      col_g=cols[2,]
      col_b=cols[3,]
    }

    #Create colour ranges
    if(any(is.na(plot.colour.ranges))) {
      plot.colour.ranges=list(xrange)
    }
    col_ranges=plot.colour.yranges
    if(any(col_ranges=='auto')) {
      col_ranges=list()
      for(r in plot.colour.ranges) {
        crange=c(Inf,-Inf)
        for(i in n:2) {
          cy=nmrdata[,i][x<r[[1]]&x>r[[2]]]
          mincy=min(cy)
          maxcy=max(cy)
          if(mincy<crange[[1]]) crange[[1]]=signif(mincy,5)
          if(maxcy>crange[[2]]) crange[[2]]=signif(maxcy,5)
        }
        col_ranges=append(col_ranges,list(crange))
      }
    }

    #Show legend
    if(colour.legend) {
      col_max_range=c(min(unlist(col_ranges,use.names=F),na.rm=T),
                      max(unlist(col_ranges,use.names=F),na.rm=T))
      #Find x coordinates of new space
      xmax=grconvertX(1, from = "ndc", to = "user")
      xleg=xmax+abs(xinch()*0.4)
      xleg2=xmax+abs(xinch()*0.1)
      xlab=(xleg+xleg2)/2
      lab_bottom=yrange[[1]]-yinch()*0.1
      lab_top=yrange[[2]]+yinch()*0.1

      zero_in_range=col_max_range[[1]]<0&col_max_range[[2]]>0
      #Disable plot clipping
      xpd=par("xpd")
      par(xpd=NA)
      #Draw legend
      if(colour.legend.show.zero&zero_in_range) {
        break_size=0.1*yinch()
        y_total_range=yrange[[2]]-yrange[[1]]
        zerofrac=col_max_range[[2]]/(col_max_range[[2]]-col_max_range[[1]])
        zeropos=y_total_range*zerofrac+yrange[[1]]
        negativecols=color.scale.jms(seq(0,zerofrac,length.out=300),col_r,col_g,col_b,xrange=c(0,1))
        positivecol=color.scale.jms(seq(zerofrac,1,length.out=300),col_r,col_g,col_b,xrange=c(0,1))
        color.legend.jms(xleg,yrange[[1]],xleg2,zeropos-break_size,"",rect.col=negativecols,align="rb",gradient="y")
        color.legend.jms(xleg,zeropos+break_size,xleg2,yrange[[2]],"",rect.col=positivecol,align="rb",gradient="y")
        text(xlab,zeropos,'0')
      } else {
        color.legend.jms(xleg,yrange[[1]],xleg2,yrange[[2]],"",rect.col=color.gradient(col_r,col_g,col_b,nslices=300),align="rb",gradient="y")
      }
      #Add labels
      text(xlab,lab_bottom,"Min")
      text(xlab,lab_top,"Max")
      par(xpd=xpd)
    }
  }

  #loop over scans to plot (reverse order)
  for(i in n:2) {
    #calculate offset for scan
    off=pos[[(i-1)]]
    #add offset to intensity
    y=nmrdata[,i]+off
    #plot scan
    if(plot.colour) {
      allx=x
      ally=y
      for(r in 1:length(plot.colour.ranges)) {
        prx=plot.colour.ranges[[r]]
        pry=y[x<prx[[1]]&x>prx[[2]]]
        prx=x[x<prx[[1]]&x>prx[[2]]]

        ally[allx %in% prx] <- NA

        cr=col_ranges[[r]]

        cvar=pry-off
        cvar=signif(cvar,5)
        cvar[cvar<cr[[1]]]<-NA
        cvar[cvar>cr[[2]]]<-NA#cr[[2]]

        cols=color.scale.jms(cvar,col_r,col_g,col_b,xrange=cr,na.color=col_na)

        nseg=length(prx)-1
        segments(prx[1:nseg], pry[1:nseg], prx[2:(nseg + 1)], pry[2:(nseg + 1)], col = cols,lwd=lwd)
      }

      nseg=length(allx)
      segments(allx[1:nseg], ally[1:nseg], allx[2:(nseg + 1)], ally[2:(nseg + 1)], col = col,lwd=lwd)
    } else {
      lines(x,y,col=col_na,lwd=lwd)
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
      xs3=seq(xs[[1]], xs[[2]], length.out = 101)
      ys3=lapply(xs3,function(x) sin((x-xs[[1]])*pi/y_trunc_sin_period)*y_trunc/y_trunc_amp_div+y_trunc)
      xs3=append(xs3,c(xs[[2]],xs[[1]]))
      ys3=append(ys3,c(yrange[[2]],yrange[[2]]))
      polygon(xs3,ys3,col='white',border=NA)

      #draw truncation line
      curve(sin((x-xs[[1]])*pi/y_trunc_sin_period)*y_trunc/y_trunc_amp_div+y_trunc,from=xs[[1]],to=xs[[2]],col=y_trunc_line_col,add=T,lwd=y_trunc_lwd)
      #add label
      #Note reversed axis == negative string width!!
      if(xcent-(strwidth(y_trunc_labels[i],cex=y_trunc_cex)+strwidth(y_trunc_labels[i],cex=y_trunc_cex)/2*0.1)/2>xrange[[1]]) {
        xcent=xrange[[1]]+strwidth(y_trunc_labels[i],cex=y_trunc_cex)/2+strwidth(y_trunc_labels[i],cex=y_trunc_cex)/2*0.1
      } else if(xcent+(strwidth(y_trunc_labels[i],cex=y_trunc_cex)+strwidth(y_trunc_labels[i],cex=y_trunc_cex)/2*0.1)/2<xrange[[2]]) {
        xcent=xrange[[2]]-strwidth(y_trunc_labels[i],cex=y_trunc_cex)/2-strwidth(y_trunc_labels[i],cex=y_trunc_cex)/2*0.1
      }
      text(xcent,ycent,y_trunc_labels[i],col=y_trunc_text_col,cex=y_trunc_cex)
    }
  }
  #make plot pretty
  box()

  if(show_axes) {
    ticksat=seq(xrange[[1]],xrange[[2]],xTickInterval*sign(xrange[[2]]-xrange[[1]]))
    minorTicksat=seq(xrange[[1]],xrange[[2]],xMinorTickInterval*sign(xrange[[2]]-xrange[[1]]))

    minorTicksat=minorTicksat[!(minorTicksat %in% ticksat)]

    #Add minor ticks
    axis(side = 1, tcl = -.2, lwd=0,lwd.ticks=1,at=minorTicksat, labels = NA)
    #Add major ticks
    axis(side = 1, tcl = -.4, lwd=0,lwd.ticks=1,at=ticksat, labels = NA)

    ##Add axis With labels (With reduced spacing from axis -- line=.4)
    lab = ticksat
    if(!show_RH_Tick)
      if(xrange[[2]]==lab[[length(lab)]])
        lab[[length(lab)]]=''
    if(!show_LH_Tick)
      if(xrange[[1]]==lab[[1]])
        lab[[1]]=''
    axis(side = 1, lwd = 0, tcl = -0.5, line = xaxismline, las = 1,at=ticksat,labels=lab)

    if(nucleus=='Unknown Nucleus') {
      axis_label=expression('Chemical Shift / ppm')
    } else {
      element=sub('[0-9]*([A-Za-z]*)','\\1',nucleus)
      mass_number=sub('([0-9]*)[A-Za-z]*','\\1',nucleus)
      axis_label=parse(text=paste0('delta*"("^"',mass_number,'"*"',element,') / ppm"'))
    }

    ##Add axis titles
    mtext(side = 1, axis_label, line = xaxislabelmline)
  }
  #return alignment parameters
  invisible(list(time_scan_1=offsets[[1]], offset_scan_1=pos[[1]],time_scan_last=offsets[[n-1]], offset_scan_last=pos[[n-1]], yrange=yrange))

}
