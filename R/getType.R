#' Get the type of an object.
#' @param x the object for which to determine the type.
#' @name getType
#' @rdname getType
NULL

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

#' @rdname getType
#' @examples
#' getType(gtGeoms$polygon)
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getType",
          signature = "geom",
          definition = function(x){
            c("vector", x@type)
          }
)

#' @rdname getType
#' @examples
#'
#' getType(x = gtSP$SpatialPolygons)
#' @importFrom methods as
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getType",
          signature = signature("Spatial"),
          definition = function(x){
            c("vector", class(x)[1])

          }
)

#' @rdname getType
#' @examples
#'
#' getType(gtSF$multiline)
#' @importFrom tibble tibble as_tibble
#' @importFrom sf st_geometry_type
#' @export
setMethod(f = "getType",
          signature = "sf",
          definition = function(x){
            c("vector", unique(as.character(st_geometry_type(x))))
          }
)

#' @rdname getType
#' @examples
#'
#' getType(gtRasters$categorical)
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getType",
          signature = "RasterLayer",
          definition = function(x){
            c("raster", class(x)[1])
          }
)