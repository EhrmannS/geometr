#' Get the type of a spatial object.
#'
#' @param x the object for which to determine the type.
#' @return A vector of two values of the geometry type (point/line/polygon/grid)
#'   and the specific main type/class of \code{x}.
#' @family getters
#' @name getType
#' @rdname getType
NULL

# generic ----
#' @rdname getType
#' @name getType
#' @export
if(!isGeneric("getType")){
  setGeneric(name = "getType",
             def = function(x, ...){
               standardGeneric("getType")
             }
  )
}

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
#' @examples
#' getType(x = gtGeoms$point)
#' @export
setMethod(f = "getType",
          signature = "geom",
          definition = function(x){
            c(x@type, x@type)
          }
)

# Spatial ----
#' @rdname getType
#' @examples
#'
#' getType(x = gtSP$SpatialPolygons)
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
#' @examples
#'
#' getType(x = gtSF$multiline)
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

# RasterLayer ----
#' @rdname getType
#' @examples
#'
#' getType(x = gtRasters$categorical)
#' @export
setMethod(f = "getType",
          signature = "Raster",
          definition = function(x){
            c("grid", class(x)[1])
          }
)

# matrix ----
#' @rdname getType
#' @examples
#'
#' getType(x = matrix(0, 3, 5))
#' @export
setMethod(f = "getType",
          signature = "matrix",
          definition = function(x){
            c("grid", "matrix")
          }
)