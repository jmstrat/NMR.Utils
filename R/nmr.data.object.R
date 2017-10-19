#' @export
`[.nmr2d.data.object` <- function(x,...) {
  r<-`[.nmr2d.data.object.super`(x,...)
  if(length(attributes(r)[['y_column']])<2) {
    #Result now a 1D scan -- change class
    classR=class(r)
    class(r)<-classR[!(classR=='nmr2dinsitu.data.object'|classR=='nmr2d.data.object')]
  }
  r
}

as.nmr2d.data.object <- function(...) {
  df=as.nmr2d.data.object.super(...)
  ycols=1:ncol(df)
  ycols=ycols[!ycols%in%attr(df,'x_column')]
  attr(df,'y_column')<-ycols
  df
}
