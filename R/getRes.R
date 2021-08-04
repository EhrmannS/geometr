#' Get the spatial resolution of a spatial object.
#'
#' @param x the object from which to derive the resolution.
#' @param ... other arguments.
#' @return A vector of two values of the spatial resolution of \code{x} in x and
#'   y dimension.
#' @family getters
#' @name getRes
#' @rdname getRes
NULL

# generic ----
#' @rdname getRes
#' @name getRes
#' @export
setGeneric(name = "getRes",
           def = function(x, ...){
             standardGeneric("getRes")
           }
)

# any ----
#' @rdname getRes
#' @export
setMethod(f = "getRes",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getRes
#' @examples
#' getRes(x = gtGeoms$grid$continuous)
#' @export
setMethod(f = "getRes",
          signature = "geom",
          definition = function(x){

            temp <- x
            if(temp@type == "grid"){
              out <- c(temp@point$x[3], temp@point$y[3])
            } else {
              out <- NULL
            }

            return(out)
          }
)

# Raster* ----
#' @rdname getRes
#' @examples
#'
#' getRes(x = gtRasters$categorical)
#' @importFrom tibble tibble
#' @importFrom raster res
#' @export
setMethod(f = "getRes",
          signature = "Raster",
          definition = function(x){
            temp <- res(x)
            out <- c(temp[1], temp[2])
            return(out)
          }
)

# matrix ----
#' @rdname getRes
#' @examples
#'
#' getRes(x = matrix(0, 3, 5))
#' @importFrom tibble tibble
#' @export
setMethod(f = "getRes",
          signature = "matrix",
          definition = function(x){
            out <- c(1, 1)
            return(out)
          }
)
