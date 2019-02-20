#' Set the reference window
#'
#' Set the reference window of a spatial object.
#' @name setWindow
NULL

#' @rdname setWindow
#' @param x the object in which to set the reference window.
#' @param to [\code{data.frame(1)}]\cr two oposing corners or all four
#'   corners of the rectangle.
#' @param ... other arguments.
#' @export
if(!isGeneric("setWindow")){
  setGeneric(name = "setWindow",
             def = function(x, to, ...){
               standardGeneric("setWindow")
             }
  )
}

#' @rdname setWindow
#' @importFrom tibble tibble
#' @export
setMethod(f = "setWindow",
          signature = "geom",
          definition = function(x, to){
            stopifnot(all(c("x", "y") %in% colnames(to)))
            if(nrow(to) == 4){
              x@window <- to[c("x", "y")]
            } else if(nrow(to) == 2){
              x@window <- tibble(x = rep(to$x, each = 2),
                                 y = c(to$y, rev(to$y)))
            } else{
              stop("no suitable window provided.")
            }
            return(x)
          }
)
