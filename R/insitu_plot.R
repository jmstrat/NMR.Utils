#' Plot electrochemisty data (vertically)
#'
#' This function plots in-situ echem data.
#' @param nmr Data frame of nmr data
#' @param echem Data frame of echem data
#' @param ... Further parameters will be passed to \code{\link[NMR.Utils]{plotNMR2D}} or \code{\link[NMR.Utils]{plotechem_vertical}}
#' @export
#' @examples
#' plotinsitu(data,echem,xrange=c(1500,-500), yrange='auto', plot_offset=100000, plot.colour.ranges=list(c(210,900)), plot.colour.yranges=list(c(50000,200000)),V_range=c(0,2),alignment=align)
plotinsitu <-function(nmr,echem,widths=c(5,1), ...) {
  args=list(...)
  echem_args=args[names(args) %in% names(formals(NMR.Utils::plotechem_vertical))]
  NMR_args=args[names(args) %in% names(formals(NMR.Utils::plotNMR2D))]
  
  echem_args=append(list(echem,tick_size_multiplier=widths[[1]]/widths[[2]]),echem_args)
  NMR_args=append(list(nmr,show_RH_Tick=FALSE),NMR_args)
  
  #Store par settings
  original_par=par('mar','xpd')
  
  #adjacent horizontal plot width ratio 5:1
  layout(matrix(c(1, 2), 1, 2, byrow=F), widths=widths)
  #set margins for LH plot to leave no margin on RHS
  par(mar=c(3,3,1,0),xpd=FALSE)
  #plot NMR data and recieve time and offset of first and last scan plotted
  align=do.call(plotNMR2D,NMR_args)
  echem_args=append(echem_args,list(alignment=align))
  #set margins of RH plot to leave no margin on LHS
  par(mar=c(3,0,1,3))
  #Plot echem
  do.call(plotechem_vertical,echem_args)
  
  layout(1)
  par(original_par)
}