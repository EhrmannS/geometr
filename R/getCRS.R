#' Get the coordinate reference system
#'
#' Standardised function to determine the coordinate reference system of a
#' spatial object.
#' @name getCRS
NULL

#' @rdname getCRS
#' @param x the object from which to extract the coordinate reference system.
#' @export
setGeneric(name = "getCRS",
           def = function(x, ...){
             standardGeneric("getCRS")
           }
)

#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature = "geom",
          definition = function(x){
            x@crs
          }
)

#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature =  signature("Spatial"),
          definition = function(x){
            as.character(x@proj4string)
          }
)

#' @rdname getCRS
#' @importFrom sf st_crs
#' @export
setMethod(f = "getCRS",
          signature = "sf",
          definition = function(x){
            st_crs(x)$proj4string
          }
)