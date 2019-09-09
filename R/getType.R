#' Get the type of a spatial object.
#'
#' @param x the object for which to determine the type.
#' @return A vector of two values giving the general type (vector/raster) and
#'   the specific type/class of \code{x}.
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

# geom ----
#' @rdname getType
#' @examples
#' getType(gtGeoms$polygon)
#' @export
setMethod(f = "getType",
          signature = "geom",
          definition = function(x){
            c("vector", x@type)
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
            c("vector", class(x)[1])

          }
)

# sf ----
#' @rdname getType
#' @examples
#'
#' getType(gtSF$multiline)
#' @importFrom sf st_geometry_type
#' @export
setMethod(f = "getType",
          signature = "sf",
          definition = function(x){
            c("vector", unique(as.character(st_geometry_type(x))))
          }
)

# ppp ----
#' @rdname getType
#' @examples
#'
#' # getType(gtPPP$...)
#' @export
setMethod(f = "getType",
          signature = "ppp",
          definition = function(x){
            c("vector", class(x)[1])
          }
)

# RasterLayer ----
#' @rdname getType
#' @examples
#'
#' getType(gtRasters$categorical)
#' @export
setMethod(f = "getType",
          signature = "RasterLayer",
          definition = function(x){
            c("raster", class(x)[1])
          }
)