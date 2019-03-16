#' Get the extent (bounding box) of a spatial object.
#' @param x the object from which to derive the extent.
#' @name getExtent
#' @rdname getExtent
NULL

#' @rdname getExtent
#' @export
if(!isGeneric("getExtent")){
  setGeneric(name = "getExtent",
             def = function(x, ...){
               standardGeneric("getExtent")
             }
  )
}

#' @rdname getExtent
#' @examples
#' getTable(gtGeoms$polygon)
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getExtent",
          signature = "geom",
          definition = function(x){
            bind_cols(x = c(min(x@vert$x), max(x@vert$x)),
                      y = c(min(x@vert$y), max(x@vert$y)))
          }
)

#' @rdname getExtent
#' @examples
#'
#' getExtent(gtSP$SpatialPolygons)
#' @importFrom raster extent
#' @importFrom tibble tibble
#' @export
setMethod(f = "getExtent",
          signature = signature("Spatial"),
          definition = function(x){
            ext <- extent(x)
            tibble(x = c(ext@xmin, ext@xmax),
                   y = c(ext@ymin, ext@ymax))
          }
)

#' @rdname getExtent
#' @examples
#'
#' getExtent(gtSF$multilinestring)
#' @importFrom sf st_bbox
#' @importFrom tibble tibble
#' @export
setMethod(f = "getExtent",
          signature = "sf",
          definition = function(x){
            ext <- st_bbox(x)
            tibble(x = c(ext[[1]], ext[[3]]),
                   y = c(ext[[2]], ext[[4]]))
          }
)

#' @rdname getExtent
#' @examples
#'
#' getExtent(gtRasters$categorical)
#' @importFrom raster extent
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getExtent",
          signature = "Raster",
          definition = function(x){
            ext <- extent(x)
            bind_cols(x = c(ext@xmin, ext@xmax),
                      y = c(ext@ymin, ext@ymax))
          }
)

#' @rdname getExtent
#' @examples
#'
#' getExtent(matrix(0, 3, 5))
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getExtent",
          signature = "matrix",
          definition = function(x){
            bind_cols(x = c(0, ncol(x)),
                      y = c(0, nrow(x)))
          }
)
