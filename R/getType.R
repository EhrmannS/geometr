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
#' getType(x = gtGeoms$polygon)
#' @export
setMethod(f = "getType",
          signature = "geom",
          definition = function(x){
            if(x@type == "grid"){
              c("raster", x@type)
            } else {
              c("vector", x@type)
            }
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
#' getType(x = gtSF$multiline)
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
#' getType(x = gtRasters$categorical)
#' @export
setMethod(f = "getType",
          signature = "Raster",
          definition = function(x){
            c("raster", class(x)[1])
          }
)

# matrix ----
#' @rdname getType
#' @export
setMethod(f = "getType",
          signature = "matrix",
          definition = function(x){
            c("raster", "matrix")
          }
)