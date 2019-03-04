#' Get the reference window of a spatial object.
#' @param x the object from which to derive the reference window.
#' @name getWindow
#' @rdname getWindow
NULL

#' @rdname getWindow
#' @export
if(!isGeneric("getWindow")){
  setGeneric(name = "getWindow",
             def = function(x, ...){
               standardGeneric("getWindow")
             }
  )
}

#' @rdname getWindow
#' @examples
#'
#' getWindow(gtGeoms$line)
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getWindow",
          signature = "geom",
          definition = function(x){
            as_tibble(x@window)
          }
)