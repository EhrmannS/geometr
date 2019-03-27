#' Get the coordinate reference system of a spatial object.
#' @param x the object from which to extract the coordinate reference system.
#' @name getCRS
#' @rdname getCRS
NULL

#' @rdname getCRS
#' @name getCRS
#' @export
if(!isGeneric("getCRS")){
  setGeneric(name = "getCRS",
             def = function(x, ...){
               standardGeneric("getCRS")
             }
  )
}

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

#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature = 'Raster',
          definition = function(x){
            as.character(x@crs)
          }
)