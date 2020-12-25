#' Get the number of rows of a spatial object.
#'
#' @param x the object from which to get the number of rows.
#' @return An integer of the number of rows.
#' @family getters
#' @name getRows
#' @rdname getRows
NULL

# generic ----
#' @rdname getRows
#' @name getRows
#' @export
if(!isGeneric("getRows")){
  setGeneric(name = "getRows",
             def = function(x, ...){
               standardGeneric("getRows")
             }
  )
}

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

# RasterLayer ----
#' @rdname getRows
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
#' @export
setMethod(f = "getRows",
          signature = "matrix",
          definition = function(x){
            out <- nrow(x)
            return(out)
          }
)
