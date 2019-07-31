#' Get the reference window of a spatial object.
#'
#' @param x the object from which to derive the reference window.
#' @return A table of the corners of the reference window of \code{x}.
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
            tibble(x = c(ext@xmin, ext@xmax, ext@xmax, ext@xmin, ext@xmin),
                   y = c(ext@ymin, ext@ymin, ext@ymax, ext@ymax, ext@ymin))
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
            tibble(x = c(ext[[1]], ext[[3]], ext[[3]], ext[[1]], ext[[1]]),
                   y = c(ext[[2]], ext[[2]], ext[[4]], ext[[4]], ext[[2]]))
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
            bind_cols(x = c(ext@xmin, ext@xmax, ext@xmax, ext@xmin, ext@xmin),
                      y = c(ext@ymin, ext@ymin, ext@ymax, ext@ymax, ext@ymin))
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
            bind_cols(x = c(0, ncol(x), ncol(x), 0, 0),
                      y = c(0, 0, nrow(x), nrow(x), 0))
          }
)