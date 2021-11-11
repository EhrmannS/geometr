#' Get the coordinate reference system of a spatial object.
#'
#' @param x the object from which to extract the coordinate reference system.
#' @param ... other arguments.
#' @return The coordinate reference system of \code{x} given as proj4string.
#' @family getters
#' @examples
#' obj <- gtGeoms$line %>%
#'   setCRS("+proj=longlat +datum=WGS84 +no_defs")
#'
#' getCRS(obj)
#'
#' gc_sp(obj) %>%
#'   getCRS()
#'
#' gc_sf(obj) %>%
#'   getCRS()
#'
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getCRS()
#'
#' gc_terra(gtGeoms$grid$categorical) %>%
#'   getCRS()
#'
#' getCRS(x = matrix(0, 3, 5))
#' @name getCRS
#' @rdname getCRS
NULL

# generic ----
#' @rdname getCRS
#' @name getCRS
#' @export
setGeneric(name = "getCRS",
           def = function(x, ...){
             standardGeneric("getCRS")
           }
)

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

# raster ----
#' @rdname getCRS
#' @export
setMethod(f = "getCRS",
          signature = 'Raster',
          definition = function(x){
            as.character(x@crs)
          }
)

# terra ----
#' @rdname getCRS
#' @importFrom terra crs
#' @export
setMethod(f = "getCRS",
          signature = 'SpatRaster',
          definition = function(x){
            crs(x, proj = TRUE)
          }
)