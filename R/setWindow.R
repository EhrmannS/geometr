#' Set the reference window of a spatial object.
#' @param x the object in which to set the reference window.
#' @param to [\code{data.frame(1)}]\cr two oposing corners.
#' @examples
#' visualise(geom = gtGeoms$mask)
#'
#' buffered <- data.frame(x = c(5074000, 5110000),
#'                        y = c(4050800, 4076000))
#' visualise(geom = setWindow(x = gtGeoms$mask,
#'                            to = buffered))
#' @name setWindow
#' @rdname setWindow
NULL

#' @rdname setWindow
#' @export
if(!isGeneric("setWindow")){
  setGeneric(name = "setWindow",
             def = function(x, to, ...){
               standardGeneric("setWindow")
             }
  )
}

#' @rdname setWindow
#' @importFrom checkmate testSubset assert
#' @importFrom tibble tibble
#' @export
setMethod(f = "setWindow",
          signature = "geom",
          definition = function(x, to){
            if("Extent" %in% class(to)){
              xVals <- c(to@xmin, to@xmax)
              yVals <- c(to@ymin, to@ymax)
            } else if(is.data.frame(to)){
              assertDataFrame(x = to, nrows = 2, min.cols = 2)
              assertNames(names(to), must.include = c("x", "y"))
              xVals <- c(to$x[1], to$x[2])
              yVals <- c(to$y[1], to$y[2])
            } else if(is.vector(to) | "bbox" %in% class(to)){
              names(to) <- tolower(names(to))
              assertNames(names(to), must.include = c("xmin", "xmax", "ymin", "ymax"))
              xVals <- c(to["xmin"], to["xmax"])
              yVals <- c(to["ymin"], to["ymax"])
            } else{
              stop("no suitable window provided.")
            }
            x@window <- tibble(x = rep(xVals, each = 2),
                               y = c(yVals, rev(yVals)))
            return(x)
          }
)
