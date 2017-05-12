#' Plot electrochemisty data (vertically)
#'
#' This function plots in-situ echem data.
#' @param nmr Data frame of nmr data
#' @param echem Data frame of echem data
#' @param ... Further parameters will be passed to \code{\link[NMR.Utils]{plot.nmr.2Ddata.object}} or \code{\link[NMR.Utils]{plotechem_vertical}}
#' @export
#' @examples
#' plot(data,xrange=c(1500,-500), yrange='auto', plot_offset=100000, plot.colour.ranges=list(c(210,900)), plot.colour.yranges=list(c(50000,200000)),V_range=c(0,2),alignment=align)
plot.insitu.nmr.2Ddata.object <-function(nmr,widths=c(3,1), ...) {
  if(!load_or_install('Echem',optional=TRUE)) stop('In situ NMR processing requires the Echem library')
  echem=attr(nmr,'echem')
  if(!is.echem.data.object(echem)) stop('Echem data is invalid')
  args=list(...)
  echem_args=args[names(args) %in% names(formals(Echem::plot_echem_vertical))]
  NMR_args=args[names(args) %in% names(formals(getS3method('plot', 'nmr.2Ddata.object')))]

  echem_args=append(list(echem,tick_size_multiplier=widths[[1]]/widths[[2]]),echem_args)
  NMR_args=append(list(nmr,show_RH_Tick=FALSE),NMR_args)

  #Store par settings
  original_par=par('mar','xpd','omi')

  #adjacent horizontal plot width ratio 5:1
  layout(matrix(c(1, 2), 1, 2, byrow=F), widths=widths)
  #set margins for LH plot to leave no margin on RHS
  par(mai=c(0.5,0.5,0.5,0),xpd=FALSE)
  #plot NMR data and recieve time and offset of first and last scan plotted
  align=do.call(getS3method('plot', 'nmr.2Ddata.object'),NMR_args)
  echem_args=append(echem_args,list(time_start=align$time_scan_1,
                                    time_end=align$time_scan_last,
                                    yrange=align$yrange,
                                    offset_start=align$offset_scan_1,
                                    offset_end=align$offset_scan_last))
  #set margins of RH plot to leave no margin on LHS
  par(mai=c(0.5,0,0.5,0.5))
  #Plot echem
  do.call(plot_echem_vertical,echem_args)

  layout(1)
  par(original_par)
}
