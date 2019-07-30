#' Set the reference window of a spatial object.
#' @param x the object in which to set the reference window.
#' @param to [\code{data.frame(1)}]\cr two oposing corners.
#' @name setWindow
#' @rdname setWindow
NULL

#' @rdname setWindow
#' @name setWindow
#' @export
if(!isGeneric("setWindow")){
  setGeneric(name = "setWindow",
             def = function(x, to, ...){
               standardGeneric("setWindow")
             }
  )
}

#' @rdname setWindow
#' @examples
#' # create a polygon programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' (aGeom <- gs_polygon(anchor = coords))
#'
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' (aGeom <- setWindow(x = aGeom, to = window))
#'
#' visualise(geom = aGeom, new = FALSE)
#' @importFrom checkmate testSubset assert
#' @importFrom tibble tibble
#' @export
setMethod(f = "setWindow",
          signature = "geom",
          definition = function(x, to){
            assertClass(x = x, classes = "geom")
            if("Extent" %in% class(to)){
              xVals <- c(to@xmin, to@xmax)
              yVals <- c(to@ymin, to@ymax)
            } else if(is.data.frame(to)){
              to <- .testWindow(x = to)
              assertDataFrame(x = to, nrows = 5, min.cols = 2)
              assertNames(names(to), must.include = c("x", "y"))
              xVals <- c(min(to$x), max(to$x))
              yVals <- c(min(to$y), max(to$y))
            } else if("bbox" %in% class(to)){
              names(to) <- tolower(names(to))
              assertNames(names(to), must.include = c("xmin", "xmax", "ymin", "ymax"))
              xVals <- c(to["xmin"], to["xmax"])
              yVals <- c(to["ymin"], to["ymax"])
            } else{
              stop("no suitable window provided.")
            }
            x@window <- tibble(x = c(rep(xVals, each = 2), xVals[1]),
                               y = c(yVals, rev(yVals), yVals[1]))
            return(x)
          }
)
