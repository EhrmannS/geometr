#' Get the type of a spatial object.
#'
#' @param x the object for which to determine the type.
#' @param ... other arguments.
#' @return A vector of two values of the geometry type (point/line/polygon/grid)
#'   and the specific main type/class of \code{x}.
#' @family getters
#' @examples
#' getType(x = gtGeoms$point)
#'
#' gc_sp(gtGeoms$line) %>%
#'   getType()
#'
#' gc_sf(gtGeoms$polygon) %>%
#'   getType()
#'
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getType()
#'
#' gc_terra(gtGeoms$grid$categorical) %>%
#'   getType()
#'
#' getType(x = matrix(0, 3, 5))
#' @name getType
#' @rdname getType
NULL

# generic ----
#' @rdname getType
#' @name getType
#' @export
setGeneric(name = "getType",
           def = function(x, ...){
             standardGeneric("getType")
           }
)

# any ----
#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "geom",
          definition = function(x){
            c(x@type, x@type)
          }
)

# Spatial ----
#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = signature("Spatial"),
          definition = function(x){
            theType <- class(x)[1]
            if(theType %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialMultiPoints", "SpatialMultiPointsDataFrame", "SpatialPixels", "SpatialPixelsDataFrame")){
              geomType <- "point"
            } else if(theType %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialGrid", "SpatialGridDataFrame")){
              geomType <- "polygon"
            } else if(theType %in% c("SpatialLines", "SpatialLinesDataFrame")){
              geomType <- "line"
            }
            c(geomType, theType)
          }
)

# sf ----
#' @rdname getType
#' @importFrom sf st_geometry_type
#' @export
setMethod(f = "getType",
          signature = "sf",
          definition = function(x){
            theType <- unique(as.character(st_geometry_type(x)))
            if(theType %in% c("POINT", "MULTIPOINT")){
              geomType <- "point"
            } else if(theType %in% c("POLYGON", "MULTIPOLYGON")){
              geomType <- "polygon"
            } else if(theType %in% c("LINESTRING", "MULTILINESTRING")){
              geomType <- "line"
            }
            c(geomType, theType)
          }
)

# raster ----
#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "Raster",
          definition = function(x){
            c("grid", class(x)[1])
          }
)

# terra ----
#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "SpatRaster",
          definition = function(x){
            c("grid", class(x)[1])
          }
)

# matrix ----
#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "matrix",
          definition = function(x){
            c("grid", class(x)[1])
          }
)