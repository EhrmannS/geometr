#' Set the reference window of a spatial object.
#'
#' @param x the object for which to set a new reference window.
#' @param to [\code{data.frame(1)}]\cr a datastructure that contains the minimum
#'   and maximum values in x and y-dimension to which the reference window shall
#'   be set.
#' @details Possible datastructures are \itemize{ \item an object of class
#'   \code{Extent}, \item an object of class \code{bbox}, \item a table with two
#'   columns (named x and y) containing the minimum and maximum values for each
#'   dimension.}
#' @return The object \code{x} with an update reference window.
#' @name setWindow
#' @rdname setWindow
NULL

# generic ----
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

# geom ----
#' @rdname setWindow
#' @examples
#' # create a polygon programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' (aGeom <- gs_polygon(anchor = coords))
#' visualise(geom = aGeom)
#'
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' (aGeom <- setWindow(x = aGeom, to = window))
#'
#' visualise(geom = aGeom)
#' @importFrom checkmate testSubset assert
#' @importFrom tibble tibble
#' @export
setMethod(f = "setWindow",
          signature = "geom",
          definition = function(x, to = NULL){
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

# ppp ----
#' @rdname setWindow
#' @examples
#'
#' window <- data.frame(x = c(0, 2),
#'                      y = c(0, 2))
#' # setWindow(x = gtPPP$..., to = window)
#' @importFrom spatstat owin
#' @export
setMethod(f = "setWindow",
          signature = "ppp",
          definition = function(x, to = NULL){
            assertNames(names(to), must.include = c("x", "y"))
            temp <- x
            aWindow <- owin(xrange = to$x, yrange = to$y)
            temp$window <- aWindow
            return(temp)
          }
)
