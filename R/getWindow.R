#' Get the reference window of a spatial object.
#'
#' The extent of an area that encompasses an object, which is at least the
#' objects extent, or larger.
#' @param x the object from which to derive the reference window.
#' @param ... other arguments.
#' @details Calling \code{getWindow} on spatial classes that do not have a
#'   window attribute returns the same value as \code{getExtent}.
#' @return A tibble of the corner coordinates of the reference window of
#'   \code{x}. This table has two columns (x and y) and two rows (minimum and
#'   maximum).
#' @family getters
#' @examples
#'
#' getWindow(gtGeoms$line)
#'
#' gc_sp(gtGeoms$line) %>%
#'   getWindow()
#'
#' gc_sf(gtGeoms$line) %>%
#'   getWindow()
#'
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getWindow()
#'
#' gc_terra(gtGeoms$grid$categorical) %>%
#'   getWindow()
#'
#' getWindow(x = matrix(0, 3, 5))
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

# raster ----
#' @rdname getWindow
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

# terra ----
#' @rdname getWindow
#' @importFrom terra ext
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getWindow",
          signature = "SpatRaster",
          definition = function(x){
            ext <- ext(x)
            bind_cols(x = c(ext[1], ext[2]),
                      y = c(ext[3], ext[4]))
          }
)

# matrix ----
#' @rdname getWindow
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getWindow",
          signature = "matrix",
          definition = function(x){
            bind_cols(x = c(0, ncol(x)),
                      y = c(0, nrow(x)))
          }
)