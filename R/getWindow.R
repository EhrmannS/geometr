#' Get the reference window
#'
#' Get the reference window of a spatial object.
#' @name getWindow
NULL

#' @rdname getWindow
#' @param x the object from which to derive the reference window.
#' @param ... other arguments.
#' @export
setGeneric(name = "getWindow",
           def = function(x, ...){
             standardGeneric("getWindow")
           }
)

#' @rdname getWindow
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getWindow",
          signature = "geom",
          definition = function(x){
            as_tibble(x@window)
          }
)