#' Get the coordinate reference system of a spatial object.
#'
#' @param x the object from which to extract the coordinate reference system.
#' @return The coordinate reference system of \code{x} given as proj4string.
#' @family getters
#' @name getCRS
#' @rdname getCRS
NULL

# generic ----
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

# any ----
#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature = "ANY",
          definition = function(x){
            NA_character_
          }
)

# geom ----
#' @rdname getCRS
#' @examples
#' getCRS(x = gtGeoms$grid$continuous)
#' @export
setMethod(f = "getCRS",
          signature = "geom",
          definition = function(x){
            x@crs
          }
)

# Spatial ----
#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature =  signature("Spatial"),
          definition = function(x){
            as.character(x@proj4string)
          }
)

# sf ----
#' @rdname getCRS
#' @examples
#'
#' library(sf)
#' nc_sf <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#' getCRS(nc_sf)
#' @importFrom sf st_crs
#' @export
setMethod(f = "getCRS",
          signature = "sf",
          definition = function(x){
            st_crs(x)$proj4string
          }
)

# Raster ----
#' @rdname getCRS
#' @examples
#'
#' getCRS(x = gtRasters$categorical)
#' @export
setMethod(f = "getCRS",
          signature = 'Raster',
          definition = function(x){
            as.character(x@crs)
          }
)