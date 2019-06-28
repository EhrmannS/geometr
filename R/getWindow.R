#' Get the reference window of a spatial object.
#' @param x the object from which to derive the reference window.
#' @name getWindow
#' @rdname getWindow
NULL

#' @rdname getWindow
#' @name getWindow
#' @export
if(!isGeneric("getWindow")){
  setGeneric(name = "getWindow",
             def = function(x, ...){
               standardGeneric("getWindow")
             }
  )
}

#' @rdname getWindow
#' @examples
#'
#' getWindow(gtGeoms$line)
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getWindow",
          signature = "geom",
          definition = function(x){
            as_tibble(x@window)
          }
)

#' @rdname getWindow
#' @examples
#'
#' getWindow(gtSP$SpatialPolygons)
#' @importFrom raster extent
#' @importFrom tibble tibble
#' @export
setMethod(f = "getWindow",
          signature = signature("Spatial"),
          definition = function(x){
            ext <- extent(x)
            tibble(x = c(ext@xmin, ext@xmax),
                   y = c(ext@ymin, ext@ymax))
          }
)

#' @rdname getWindow
#' @examples
#'
#' getWindow(gtSF$multilinestring)
#' @importFrom sf st_bbox
#' @importFrom tibble tibble
#' @export
setMethod(f = "getWindow",
          signature = "sf",
          definition = function(x){
            ext <- st_bbox(x)
            tibble(x = c(ext[[1]], ext[[3]]),
                   y = c(ext[[2]], ext[[4]]))
          }
)

#' @rdname getWindow
#' @examples
#'
#' getWindow(gtRasters$categorical)
#' @importFrom raster extent
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getWindow",
          signature = "Raster",
          definition = function(x){
            ext <- extent(x)
            bind_cols(x = c(ext@xmin, ext@xmax),
                      y = c(ext@ymin, ext@ymax))
          }
)

#' @rdname getWindow
#' @examples
#'
#' getWindow(matrix(0, 3, 5))
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getWindow",
          signature = "matrix",
          definition = function(x){
            bind_cols(x = c(0, ncol(x)),
                      y = c(0, nrow(x)))
          }
)