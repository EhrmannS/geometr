#' Get the history of a spatial object.
#'
#' @param x the object from which to derive the history.
#' @return A list of the events that lead to \code{x}.
#' @family getters
#' @name getHistory
#' @rdname getHistory
NULL

# generic ----
#' @rdname getHistory
#' @name getHistory
#' @export
if(!isGeneric("getHistory")){
  setGeneric(name = "getHistory",
             def = function(x, ...){
               standardGeneric("getHistory")
             }
  )
}

# any ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "geom",
          definition = function(x){
            hist <- x@history
            return(hist)
          }
)

# RasterLayer ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "Raster",
          definition = function(x){
            if(inherits(x, "RasterStack")){
              hist <- list()
              for(i in 1:dim(x)[3]){
                hist <- c(hist, x@history)
              }
            } else {
              hist <- x@history
            }
            return(hist)
          }
)
