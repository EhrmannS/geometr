#' Get the number of columns of a spatial object.
#'
#' @param x the object from which to get the number of columns.
#' @param ... other arguments.
#' @return An integer of the number of columns.
#' @family getters
#' @examples
#'
#' getCols(gtGeoms$grid$categorical)
#'
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getCols()
#'
#' gc_terra(gtGeoms$grid$categorical) %>%
#'   getCols()
#'
#' getCols(x = matrix(0, 3, 5))
#' @name getCols
#' @rdname getCols
NULL

# generic ----
#' @rdname getCols
#' @name getCols
#' @export
setGeneric(name = "getCols",
           def = function(x, ...){
             standardGeneric("getCols")
           }
)

# any ----
#' @rdname getCols
#' @export
setMethod(f = "getCols",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getCols
#' @export
setMethod(f = "getCols",
          signature = "geom",
          definition = function(x){
            if(x@type == "grid"){
              out <- x@point$x[2]
            } else {
              out <- NULL
            }
            return(out)
          }
)

# raster ----
#' @rdname getCols
#' @importFrom raster ncol
#' @export
setMethod(f = "getCols",
          signature = "Raster",
          definition = function(x){
            out <- ncol(x)
            return(out)
          }
)

# terra ----
#' @rdname getCols
#' @importFrom terra ncol
#' @export
setMethod(f = "getCols",
          signature = "SpatRaster",
          definition = function(x){
            ncol(x)
          }
)

# matrix ----
#' @rdname getCols
#' @export
setMethod(f = "getCols",
          signature = "matrix",
          definition = function(x){
            out <- ncol(x)
            return(out)
          }
)
