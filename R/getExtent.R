#' Get the extent (bounding box) of a spatial object.
#'
#' @param x the object from which to derive the extent.
#' @return A table of the lower left and upper right corner of the extent of \code{x}.
#' @family getters
#' @name getExtent
#' @rdname getExtent
NULL

# generic ----
#' @rdname getExtent
#' @name getExtent
#' @export
if(!isGeneric("getExtent")){
  setGeneric(name = "getExtent",
             def = function(x, ...){
               standardGeneric("getExtent")
             }
  )
}

# geom ----
#' @rdname getExtent
#' @examples
#' getTable(gtGeoms$polygon)
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getExtent",
          signature = "geom",
          definition = function(x){
            bind_cols(x = c(min(x@point$x), max(x@point$x)),
                      y = c(min(x@point$y), max(x@point$y)))
          }
)

# Spatial ----
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

# sf ----
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

# ppp ----
#' @rdname getExtent
#' @examples
#'
#' getExtent(gtPPP)
#' @export
setMethod(f = "getExtent",
          signature = "ppp",
          definition = function(x){
            bla <- x
            tibble(x = c(min(bla$x), max(bla$x)),
                   y = c(min(bla$y), max(bla$y)))
          }
)

# Raster ----
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

# matrix ----
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
