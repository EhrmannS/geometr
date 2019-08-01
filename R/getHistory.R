#' Get the history of a spatial object.
#'
#' @param x the object from which to derive the history.
#' @return A list of the events that lead to \code{x}.
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

# geom ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "geom",
          definition = function(x){
            x@history
          }
)

# RasterLayer ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "RasterLayer",
          definition = function(x){
            x@history
          }
)

# RasterBrick ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "RasterBrick",
          definition = function(x){
            x@history
          }
)

# RasterStack ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "RasterStack",
          definition = function(x){
            hist <- list()
            for(i in 1:dim(x)[3]){
              hist <- c(hist, x@history)
            }
            return(hist)
          }
)
