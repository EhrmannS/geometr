#' Scale \code{geom}s
#'
#' Scale the vertex values of \code{geom}s to a values range or so that they are
#' either relative to the \code{@window} slot, or absolute values.
#' @param x [\code{geometric object(1)}]\cr the object to be scaled.
#' @param range [\code{list(2)}]\cr vector of length two for both of the
#'   \code{x} and \code{y} dimension to which the values should be scaled.
#' @return Scaled \code{geom}.
#' @family geometry tools
#' @examples
#' coords <- data.frame(x = c(40, 70, 70, 50, 40),
#'                      y = c(40, 40, 60, 70, 40),
#'                      fid = 1)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#'
#' # change to relative scale and back to absolute
#' (relCoords <- gt_scale(geom = aGeom, range = data.frame(x = c(0, 1), y = c(0, 1))))
#' gt_scale(geom = relCoords, range = window)
#'
#' # scale to another range
#' gt_scale(geom = aGeom, range = tibble(x = c(0, 100), y = c(10, 90)))
#'
#' @importFrom checkmate testList assertNames assertChoice
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @export

gt_scale <- function(x, range = NULL){

  assertDataFrame(x = range, types = "numeric", any.missing = FALSE, ncols = 2)
  assertNames(names(range), permutation.of = c("x", "y"))

  thePoints <- getPoints(x = x)
  theType <- getType(x = x)[1]

  minX <- min(thePoints$x)
  maxX <- max(thePoints$x)
  minY <- min(thePoints$y)
  maxY <- max(thePoints$y)

  if(minX == maxX){
    stop("I can't scale a 'geom' that has equal minimum and maximum x values of the window.")
  }
  if(minY == maxY){
    stop("I can't scale a 'geom' that has equal minimum and maximum y values of the window.")
  }
  thePoints$x <- (thePoints$x - minX) * (max(range$x) - min(range$x)) / (maxX - minX) +  min(range$x)
  thePoints$y <- (thePoints$y - minY) * (max(range$y) - min(range$y)) / (maxY - minY) +  min(range$y)

  if(!is.null(range)){
    window <- as.data.frame(range)
  }

  if(theType == "grid"){
    theFeatures <- getFeatures(x = x)
    theGroups <- getGroups(x = x)
  } else {
    theFeatures <- list(geometry = getFeatures(x = x))
    theGroups <- list(geometry = getGroups(x = x))
  }

  # make history
  hist <- paste0("coordinate values were rescaled between [", paste0(range$x, collapse = " "), "] (x) and [",  paste0(range$y, collapse = " "), "] (y)")

  # make new geom
  out <- new(Class = "geom",
             type = theType,
             point = thePoints,
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = x),
             scale = "absolute",
             crs = getCRS(x = x),
             history =c(getHistory(x = x), list(hist)))

  return(out)
}