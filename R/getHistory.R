#' Get the history
#'
#' Get the history of a \code{geom} object.
#' @name getHistory
NULL

#' @rdname getHistory
#' @param x the object from which to derive the history.
#' @param ... other arguments.
#' @export
if(!isGeneric("getHistory")){
  setGeneric(name = "getHistory",
             def = function(x, ...){
               standardGeneric("getHistory")
             }
  )
}

#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "geom",
          definition = function(x){
            x@history
          }
)