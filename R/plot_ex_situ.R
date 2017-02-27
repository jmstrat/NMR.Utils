#' Read and plot ex situ NMR data
#'
#' This function is used to read and plot ex situ NMR data.
#' @param files The Paths to the data files
#' @param acqus_dirs The Paths to the directories containing acqus files
#' @param ... Additional parameters passed to \code{\link{plot_ex_situ_nmr}}
#' @export
plot_ex_situ_nmr_files <- function(files,acqus_dirs=c(),...) {
  data_list=list()
  if(length(acqus_dirs) && length(files)!=length(acqus_dirs)) {
    warning("The number of files and acqus directories are not equal, ignoring the acqus files.")
    acqus_dirs=c()
  }
  for(i in 1:length(files)) {
    ac=acqus_dirs[[i]]
    if(is.null(ac)) ac=NA
    data_list=append(data_list,read.NMR(file,acqus=ac))
  }
  plot_ex_situ_nmr(data_list,...)
}

#' Plot ex situ NMR data
#'
#' This function is used to plot ex situ NMR data.
#' @param data A list of data to plot (see \code{\link{read.NMR}})
#' @param names Names of the data to go into the legend (defaults to filenames)
#' @param plot.cols list of colours for plots (defaults to automatically chosing)
#' @return A list of plotting options to be displayed to the user (invisibly)
#' @export
#' @family Ploting Methods
plot_ex_situ_nmr <- function(data,names=c(""),plot.cols=hcl(h=15+c(1:length(cellids))*360/length(cellids),c=100,l=65), .interactive_xlim=NULL, .interactive_ylim=NULL,...)
{
  if(!is.list(data)) data=list(data)

  #We produce a grid of plots, one per nucleus in the supplied data

  ####


  #Remove cells without any data
  data_list<-data_list[unlist(lapply(data_list,function(y) !all(is.na(y))))]

  if(all(is.na(plot.cols))) plot.cols=hcl(h=15+c(1:length(data_list))*360/length(data_list),c=100,l=65)

  if(is.data.frame(data_list))
  {
    data_list=list(data_list)
  }
  if(length(data_list)==0) {
    flog.error("No data to plot")
    return(invisible())
  }

  for(d in data_list)
  {
    assert_data_columns(d,c("Discharge_SpecificCapacity.Ah.","Charge_SpecificCapacity.Ah.","SpecificCapacity.Ah.","Voltage.V."))
  }

  if(all(names==c(""))) {
    for(i in 1:length(data_list)) {
      n=attr(data_list[[i]],'filename')
      names[[i]]=n
    }
  }

  #If not specified, automatically find complimentary colours
  if(any(is.na(plot.chargecols)) || length(plot.chargecols) != length(plot.cols)) {
    plot.chargecols=rgb2hsv(col2rgb(plot.cols))
    plot.chargecols['h',]=plot.chargecols['h',]+0.05
    plot.chargecols['h',][plot.chargecols['h',]>1]=plot.chargecols['h',][plot.chargecols['h',]>1]-1
    plot.chargecols=hsv(plot.chargecols['h',],plot.chargecols['s',],plot.chargecols['v',])
  }

  tick_int_cap=20
  leg_text_width=12
  cap_dfs=lapply(data_list,get_cycle_capacities)

  dcaps=lapply(cap_dfs,function(x) x[,'Discharge_Capacity_mahg'])

  maxcycles=max(unlist(lapply(dcaps,length)))
  maxcy=max(unlist(dcaps))

  if(charge_as_coulombic)
    ccaps=lapply(cap_dfs,function(x) (x[,'Efficiency'])*maxcy/100)
  else
    ccaps=lapply(cap_dfs,function(x) x[,'Charge_Capacity_mahg'])

  ######## BEGIN PLOTTING ########

  ymax=signif(maxcy,2)*1.1
  m=mo=par('mar')
  if(charge && charge_as_coulombic) {
    m[[4]]=m[[2]]
    par(mar=m)
  }

  xrange=c(1,maxcycles)
  yrange=c(0,ymax)

  if(!is.null(.interactive_xlim)) {
    flog.debug("Interactive x-range specified as %s-%s",.interactive_xlim[[1]],.interactive_xlim[[2]])
    xrange=.interactive_xlim
  }
  if(!is.null(.interactive_ylim)) {
    flog.debug("Interactive y-range specified as %s-%s",.interactive_ylim[[1]],.interactive_ylim[[2]])
    yrange=.interactive_ylim
  }
  flog.debug("Starting to plot")

  plot(0,0, type="n", xaxs="i", yaxs="i", xlim=xrange, ylim=yrange, axes = FALSE, xlab = NA, ylab = NA)
  ##PLOT DATA##

  invisible(mapply(function(x,col) lines(c(1:length(x)),x,col=col),dcaps,plot.cols))
  invisible(mapply(function(x,col) points(c(1:length(x)),x,col=col,pch=16),dcaps,plot.cols))

  if(charge) {
    invisible(mapply(function(x,col) lines(c(1:length(x)),x,col=col),ccaps,plot.chargecols))
    invisible(mapply(function(x,col) points(c(1:length(x)),x,col=col,pch=16),ccaps,plot.chargecols))
  }
  ##Add a box
  box()

  ytickInterval<-tick_interval(yrange[[2]]-yrange[[1]])
  yticksat=seq(round10(yrange[[1]]),round10(yrange[[1]])+ytickInterval*10,ytickInterval)

  yMinortickInterval<-ytickInterval/2
  yMinorticksat=seq(round10(yrange[[1]]),round10(yrange[[1]])+yMinortickInterval*20,yMinortickInterval)

  yMinorticksat=yMinorticksat[!(yMinorticksat %in% yticksat)]

  xtickInterval<-tick_interval(xrange[[2]]-xrange[[1]])
  xticksat=seq(round(xrange[[1]]),round(xrange[[1]])+xtickInterval*10,xtickInterval)

  xMinortickInterval<-xtickInterval/2
  xMinorticksat=seq(round(xrange[[1]]),round(xrange[[1]])+xMinortickInterval*20,xMinortickInterval)

  xMinorticksat=xMinorticksat[!(xMinorticksat %in% xticksat)]

  #Add minor ticks
  axis(side = 1, tcl = -.2, at=xMinorticksat, labels = NA)
  axis(side = 2, tcl = -.2, at=yMinorticksat, labels = NA)
  #Add major ticks
  axis(side = 1, tcl = -.4, at=xticksat, labels = NA)
  axis(side = 2, tcl = -.4, at=yticksat, labels = NA)

  ##Add labels (With reduced spacing from axis -- line=.4)
  axis(side = 1, lwd = 0,tcl = -0.5, line = -.6, at=xticksat)
  axis(side = 2, lwd = 0,tcl = -0.5, line = -.4, at=yticksat, las = 1)

  ##Add axis titles
  mtext(side = 1, "Cycles", line = 1.8)
  mtext(side = 2, expression("Capacity (mA h g"^-1*")"), line = 2+0.4*(nchar(ymax)-3))

  if(charge && charge_as_coulombic) {
    y2tickInterval<-tick_interval((yrange[[2]]-yrange[[1]])*100/ymax)
    y2ticksat=seq(round10(yrange[[1]]*100/maxcy),round10(yrange[[1]]*100/maxcy)+y2tickInterval*10,y2tickInterval)
    y2MinortickInterval<-y2tickInterval/2
    y2Minorticksat=seq(round10(yrange[[1]]*100/maxcy),round10(yrange[[1]]*100/maxcy)+y2MinortickInterval*20,y2MinortickInterval)


    y2scaledticksat<-y2ticksat*maxcy/100
    y2scaledMinorticksat<-y2Minorticksat*maxcy/100
    y2scaledMinorticksat=y2scaledMinorticksat[!(y2scaledMinorticksat %in% y2scaledticksat)]
    axis(side = 4, tcl = -.2, at=y2scaledMinorticksat, labels = NA)
    axis(side = 4, tcl = -.4, at=y2scaledticksat, labels = NA)
    axis(side = 4, lwd = 0,tcl = -0.5, line = -.4, at=y2scaledticksat, labels=y2ticksat, las = 1)
    mtext(side = 4, "Coulombic efficiency (%)", line = 2.1)
  }

  par(mar=mo)


  plot_capacity_data_internal_legend <- function() {
    for(n in names) {
      if(nchar(n)>13)
        n=paste0(strtrim(n,10),"...")
      names[[i]]=n
    }
    par(mar=m)
    leg_names=names
    if(length(data_list)==1 && charge) {
      #Plot basic legend (by fudging parameters for the "normal" legend)
      plot.cols=c(plot.cols[[1]],plot.chargecols[[1]])
      if(charge_as_coulombic)
        leg_names=c("Discharge","Efficiency")
      else
        leg_names=c("Discharge","Charge")
      charge=FALSE
    }

    cin <- par("cin")
    leg_top=ymax
    leg_right=maxcycles-xinch(cin[1L]*0.5, warn.log = FALSE)
    leg_voffset=yMinortickInterval
    leg_text_cex=0.7
    leg_cex=1
    leg_lwd=2

    leg_hoffset=xinch(cin[1L]*0.1, warn.log = FALSE)
    leg_lwidth=leg_cex*xinch(cin[1L], warn.log = FALSE)

    leg_lwidth2=0
    leg_hoffset2=0

    if(charge) {
      leg_lwidth2=leg_lwidth
      leg_hoffset2=leg_hoffset*2

      leg_top=leg_top-leg_voffset
      text(mean(c(leg_right-leg_hoffset2-leg_lwidth2,leg_right-leg_lwidth-leg_hoffset2-leg_lwidth2)),leg_top-leg_voffset,"D",pos=3,cex=leg_text_cex)
      text(mean(c(leg_right,leg_right-leg_lwidth)),leg_top-leg_voffset,"C",pos=3,cex=leg_text_cex)
    }

    for(i in 1:length(leg_names)) {
      text(leg_right-leg_lwidth-leg_hoffset-leg_lwidth2-leg_hoffset,leg_top-leg_voffset*i,leg_names[[i]],pos=2,cex=leg_text_cex)
      lines(c(leg_right-leg_hoffset2-leg_lwidth2,leg_right-leg_lwidth-leg_hoffset2-leg_lwidth2),c(leg_top-leg_voffset*i,leg_top-leg_voffset*i),col=plot.cols[[i]],lwd=leg_lwd)
      if(charge)
        lines(c(leg_right,leg_right-leg_lwidth),c(leg_top-leg_voffset*i,leg_top-leg_voffset*i),col=plot.chargecols[[i]],lwd=leg_lwd)
    }
    par(mar=mo)
  }
  plot_capacity_data_external_legend <- function() {
    leg_names=names
    if(length(data_list)==1 && charge) {
      #Plot basic legend (by fudging parameters for the "normal" legend)
      #cols=c(plot.cols[[1]],plot.chargecols[[1]])
      if(charge_as_coulombic)
        leg_names=c("Discharge","Efficiency")
      else
        leg_names=c("Discharge","Charge")
      #charge=FALSE
    }
    if(charge) {
      odr <- order(c(seq_along(plot.cols), seq_along(plot.chargecols)))
      cols=c(plot.cols,plot.chargecols)[odr]
    } else {
      cols=plot.cols
    }
    legend_horizontal(leg_names,cols,20,new_plot = TRUE)
  }

  plot_options = list(
    charge=list(type='logical',label="Plot charge data?", value=charge),
    charge_as_coulombic=list(type=if(charge){'logical'} else {'invisible'} ,label="Plot charge data as coulombic efficiency?", value=charge_as_coulombic),
    legend=list(
      internal=plot_capacity_data_internal_legend,
      external=plot_capacity_data_external_legend
    )
  )

  plot_options=append(plot_options,list())
  return(invisible(plot_options))
}

