#' Get the number of columns of a spatial object.
#'
#' @param x the object from which to get the number of columns.
#' @param ... other arguments.
#' @return An integer of the number of columns.
#' @family getters
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
#' @examples
#' getCols(x = gtGeoms$grid$continuous)
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

# RasterLayer ----
#' @rdname getCols
#' @examples
#'
#' getCols(x = gtRasters$categorical)
#' @importFrom raster ncol
#' @export
setMethod(f = "getCols",
          signature = "Raster",
          definition = function(x){
            out <- ncol(x)
            return(out)
          }
)

# matrix ----
#' @rdname getCols
#' @examples
#'
#' getCols(x = matrix(0, 3, 5))
#' @export
setMethod(f = "getCols",
          signature = "matrix",
          definition = function(x){
            out <- ncol(x)
            return(out)
          }
)
