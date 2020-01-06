#' Get the reference window of a spatial object.
#'
#' @param x the object from which to derive the reference window.
#' @return A table of the corners of the reference window of \code{x}.
#' @family getters
#' @name getWindow
#' @rdname getWindow
NULL

# generic ----
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
            tibble(x = c(ext@xmin, ext@xmax, ext@xmax, ext@xmin, ext@xmin),
                   y = c(ext@ymin, ext@ymin, ext@ymax, ext@ymax, ext@ymin))
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
            tibble(x = c(ext[[1]], ext[[3]], ext[[3]], ext[[1]], ext[[1]]),
                   y = c(ext[[2]], ext[[2]], ext[[4]], ext[[4]], ext[[2]]))
          }
)

# ppp ----
#' @rdname getWindow
#' @examples
#'
#' getWindow(x = gtPPP)
#' @export
setMethod(f = "getWindow",
          signature = "ppp",
          definition = function(x){
            temp <- x
            if("bdry" %in% names(temp$window)){
              verts <- do.call(rbind.data.frame, temp$window$bdry)
              fids <- rep(seq_along(temp$window$bdry), sapply(temp$window$bdry, function(x) length(x[[1]])))
              verts <- bind_cols(verts, fid = fids)
              verts <- .updateVertices(input = verts)
              verts$fid <- NULL
            } else {
              xmin <- min(temp$window$xrange)
              xmax <- max(temp$window$xrange)
              ymin <- min(temp$window$yrange)
              ymax <- max(temp$window$yrange)
              verts <- tibble(x = c(xmin, xmax, xmax, xmin, xmin),
                              y = c(ymin, ymin, ymax, ymax, ymin))
            }

            return(verts)
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
            bind_cols(x = c(ext@xmin, ext@xmax, ext@xmax, ext@xmin, ext@xmin),
                      y = c(ext@ymin, ext@ymin, ext@ymax, ext@ymax, ext@ymin))
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
            bind_cols(x = c(0, ncol(x), ncol(x), 0, 0),
                      y = c(0, 0, nrow(x), nrow(x), 0))
          }
)