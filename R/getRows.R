#' Get the number of rows of a spatial object.
#'
#' @param x the object from which to get the number of rows.
#' @param ... other arguments.
#' @return An integer of the number of rows.
#' @family getters
#' @name getRows
#' @rdname getRows
NULL

# generic ----
#' @rdname getRows
#' @name getRows
#' @export
setGeneric(name = "getRows",
           def = function(x, ...){
             standardGeneric("getRows")
           }
)

# any ----
#' @rdname getRows
#' @export
setMethod(f = "getRows",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getRows
#' @examples
#' getRows(x = gtGeoms$grid$continuous)
#' @export
setMethod(f = "getRows",
          signature = "geom",
          definition = function(x){
            if(x@type == "grid"){
              out <- x@point$y[2]
            } else {
              out <- NULL
            }
            return(out)
          }
)

# RasterLayer ----
#' @rdname getRows
#' @examples
#'
#' getRows(x = gtRasters$categorical)
#' @importFrom raster nrow
#' @export
setMethod(f = "getRows",
          signature = "Raster",
          definition = function(x){
            out <- nrow(x)
            return(out)
          }
)

# matrix ----
#' @rdname getRows
#' @examples
#'
#' getRows(x = matrix(0, 3, 5))
#' @export
setMethod(f = "getRows",
          signature = "matrix",
          definition = function(x){
            out <- nrow(x)
            return(out)
          }
)
