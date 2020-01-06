#' Get the extent (bounding box) of a spatial object.
#'
#' @param x the object from which to derive the extent.
#' @return A table of the lower left and upper right corner of the extent of
#'   \code{x}.
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

# any ----
#' @rdname getExtent
#' @export
setMethod(f = "getExtent",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getExtent
#' @examples
#' getExtent(gtGeoms$polygon)
#' @importFrom tibble tibble
#' @export
setMethod(f = "getExtent",
          signature = "geom",
          definition = function(x){

            if(x@type == "grid"){
              temp <- x
              out <- tibble(x = c(temp@point$x[1], temp@point$x[1] + temp@point$x[2]*temp@point$x[3]),
                            y = c(temp@point$y[1], temp@point$y[1] + temp@point$y[2]*temp@point$y[3]))
            } else {
              thePoints <- getPoints(x = x)
              out <- tibble(x = c(min(thePoints$x), max(thePoints$x)),
                            y = c(min(thePoints$y), max(thePoints$y)))
            }

            return(out)
          }
)

# Spatial ----
#' @rdname getExtent
#' @examples
#'
#' getExtent(x = gtSP$SpatialPolygons)
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
#' getExtent(x = gtSF$multilinestring)
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
#' getExtent(x = gtPPP)
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
#' getExtent(x = gtRasters$categorical)
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
#' getExtent(x = matrix(0, 3, 5))
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getExtent",
          signature = "matrix",
          definition = function(x){
            bind_cols(x = c(0, ncol(x)),
                      y = c(0, nrow(x)))
          }
)
