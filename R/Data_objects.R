# NOTE These functions are created in the .onLoad function and added to the namespace,
# we need to export and document them here, and add overrides to the default behaviour
# if necessary.

##' @title NMR data objects
##' @param x The object
##' @rdname data_objects_null_exports

##' @name nmr.data.object
##' @usage nmr.data.object(...)
##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
NULL

##' @name nmr2d.data.object
##' @usage nmr2d.data.object(...)
##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
NULL

##' @name nmr2dinsitu.data.object
##' @usage nmr2dinsitu.data.object(...)
##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
NULL

##' @name as.nmr.data.object
##' @usage as.nmr.data.object(x)
##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
NULL

##' @name as.nmr2d.data.object
##' @usage as.nmr2d.data.object(x)
##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
NULL

##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
as.nmr2dinsitu.data.object <- function(x) {
  if(is.nmr2d.data.object(x)) {
    x=as.nmr2dinsitu.data.object.super(x)
    attr(x, "echem") <- Echem.Data::echem.data.object()
  } else {
    stop("as.nmr2dinsitu.data.object can only be called with an nmr2d.data.object")
  }
  return(x)
}

##' @name is.nmr.data.object
##' @usage is.nmr.data.object(x)
##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
NULL

##' @name is.nmr2d.data.object
##' @usage is.nmr2d.data.object(x)
##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
NULL

##' @name is.nmr2dinsitu.data.object
##' @usage is.nmr2dinsitu.data.object(x)
##' @export
##' @rdname data_objects_null_exports
##' @keywords internal
NULL
