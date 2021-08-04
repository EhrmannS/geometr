#' Get the reference window of a spatial object.
#'
#' @param x the object from which to derive the reference window.
#' @param ... other arguments.
#' @return A tibble of the corner coordinates of the reference window of
#'   \code{x}. This table two columns (x and y) and two rows (minimum and
#'   maximum).
#' @family getters
#' @name getWindow
#' @rdname getWindow
NULL

# generic ----
#' @rdname getWindow
#' @name getWindow
#' @export
setGeneric(name = "getWindow",
           def = function(x, ...){
             standardGeneric("getWindow")
           }
)

# any ----
#' @rdname getWindow
#' @export
setMethod(f = "getWindow",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getWindow
#' @examples
#'
#' getWindow(x = gtGeoms$line)
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getWindow",
          signature = "geom",
          definition = function(x){
            as_tibble(x@window)
          }
)

# Spatial ----
#' @rdname getWindow
#' @examples
#'
#' getWindow(x = gtSP$SpatialLines)
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

# sf ----
#' @rdname getWindow
#' @examples
#'
#' getWindow(x = gtSF$multilinestring)
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

# Raster ----
#' @rdname getWindow
#' @examples
#'
#' getWindow(x = gtRasters$categorical)
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

# matrix ----
#' @rdname getWindow
#' @examples
#'
#' getWindow(x = matrix(0, 3, 5))
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getWindow",
          signature = "matrix",
          definition = function(x){
            bind_cols(x = c(0, ncol(x)),
                      y = c(0, nrow(x)))
          }
)