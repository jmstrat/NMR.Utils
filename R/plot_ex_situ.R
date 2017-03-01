#' Read and plot ex situ NMR data
#'
#' This function is used to read and plot ex situ NMR data.
#' @param files The Paths to the data files
#' @param acqus_dirs The Paths to the directories containing acqus files
#' @param ... Additional parameters passed to \code{\link{plot_ex_situ_nmr}}
#' @export
plot_ex_situ_nmr_files <- function(files,acqus_dirs=c(),masses=c(),...) {
  data_list=list()
  if(length(acqus_dirs) && length(files)!=length(acqus_dirs)) {
    warning("The number of files and acqus directories are not equal, ignoring the acqus files.")
    acqus_dirs=c()
  }
  for(i in 1:length(files)) {
    ac=acqus_dirs[[i]]
    if(is.null(ac)) ac=NA
    mass=masses[[i]]
    if(is.null(mass)) mass=NA
    data_list=append(data_list,list(read.NMR(files[[i]],acqus=ac,mass=mass)))
  }
  plot_ex_situ_nmr(data_list,...)
}

#' Plot ex situ NMR data
#'
#' This function is used to plot ex situ NMR data. A grid of plots is produced per nucleus present in the data list.
#' @param data A list of data to plot (see \code{\link{read.NMR}})
#' @param names Names of the data to go into the legend (defaults to filenames)
#' @param plot.cols list of colours for plots (defaults to automatically chosing)
#' @return A list of plotting options to be displayed to the user (invisibly)
#' @export
#' @family Ploting Methods
plot_ex_situ_nmr <- function(data,names=c(""),plot.cols=NA, .interactive_xlim=NULL, .interactive_ylim=NULL,...)
{
  if(!class(data)=='list') data=list(data)

  #We produce a grid of plots, one per nucleus in the supplied data

  nuclei=c()
  data_per_nucleus=list()
  for(d in data) {
    nucleus=attr(d,'nuc1')
    if(is.null(nucleus)) nucleus='Unknown Nucleus'
    if(!nucleus %in% nuclei) {
      nuclei=append(nuclei,nucleus)
      data_per_nucleus[[nucleus]]=list(d)
    } else {
      data_per_nucleus[[nucleus]]=append(data_per_nucleus[[nucleus]],list(d))
    }
  }

  n=length(nuclei)
  load_or_install('Plotting.Utils')
  #Setup the plots
  par(oma=c(1.6,1,0,0))
  grid.layout(n)

  #Make the plots
  for(nuc in nuclei) {
    #Set up margins
    par(mai=c(.2,.1,.1,.1))

    #Get the data for this nucleus
    data_to_plot=data_per_nucleus[[nuc]]
    #Find the plot bounds
    min_x=min(sapply(data_to_plot, function(x) min(x[,1])))
    max_x=max(sapply(data_to_plot, function(x) max(x[,1])))
    min_y=min(sapply(data_to_plot, function(x) min(x[,2])))
    max_y=max(sapply(data_to_plot, function(x) max(x[,2])))

    mass_min=min(sapply(data_to_plot, function(x) as.numeric(attr(x,'mass'))),na.rm=TRUE)
    ns_min=min(sapply(data_to_plot, function(x) as.numeric(attr(x,'ns'))),na.rm=TRUE)

    xrange=c(max_x,min_x)
    yrange=c(min_y,max_y)
    if(length(nuclei)==1){
      if(!is.null(.interactive_xlim)) {
        xrange=.interactive_xlim
      }
      if(!is.null(.interactive_ylim)) {
        yrange=.interactive_ylim
      }
    }

    #Make a blank plot
    pretty_plot(xlim=xrange,ylim=yrange,y_axis=NA,div=5)

    #Determine the line colours
    len=length(data_to_plot)
    if(any(is.na(plot.cols))) {
      if(len==1) {
        this.plot.cols=c('black')
      } else {
        this.plot.cols=hcl(h=15+c(1:len)*360/len,c=100,l=65)
      }
    } else {
      this.plot.cols=rep(plot.cols,length.out = len)
    }

    #Normalise and plot the data
    for(i in 1:len) {
      m=attr(data_to_plot[[i]],'mass')
      if(is.null(m)) m=1
      m=as.numeric(m)
      ns=attr(data_to_plot[[i]],'ns')
      if(is.null(ns)) ns=1
      ns=as.numeric(ns)
      lines(data_to_plot[[i]][,1],data_to_plot[[i]][,2]/(m/mass_min)/(ns/ns_min),col=this.plot.cols[[i]])
    }

    #Add the nucleus label
    nuc_num=sub('^([[:digit:]]*).*$','\\1',nuc)
    nuc_string=sub('^[[:digit:]]*(.*)$','\\1',nuc)
    add_plot_label(parse(text=paste0('""^"',nuc_num,'"*"',nuc_string,'"'))) #Convoluted method to superscript mass number...
  }

  #Add the axis labels
  mtext(text="Intensity",side=2,line=0,outer=TRUE)
  mtext(text="δ / ppm",side=1,line=0.6,outer=TRUE)

  return(invisible())
  #TODO:
  #Legend(s)
  #Plot options
}

