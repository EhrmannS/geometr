#' Get the number of rows of a spatial object.
#'
#' @param x the object from which to get the number of rows.
#' @param ... other arguments.
#' @return An integer of the number of rows.
#' @family getters
#' @examples
#'
#' getRows(gtGeoms$grid$categorical)
#'
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getRows()
#'
#' gc_terra(gtGeoms$grid$categorical) %>%
#'   getRows()
#'
#' getRows(x = matrix(0, 3, 5))
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

# raster ----
#' @rdname getRows
#' @importFrom raster nrow
#' @export
setMethod(f = "getRows",
          signature = "Raster",
          definition = function(x){
            nrow(x)
          }
)

# terra ----
#' @rdname getRows
#' @importFrom terra nrow
#' @export
setMethod(f = "getRows",
          signature = "SpatRaster",
          definition = function(x){
            nrow(x)
          }
)

# matrix ----
#' @rdname getRows
#' @export
setMethod(f = "getRows",
          signature = "matrix",
          definition = function(x){
            out <- nrow(x)
            return(out)
          }
)
