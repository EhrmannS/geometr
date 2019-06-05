#' Scale geometries
#'
#' Scale the vertex values of \code{geom}s so that they are either relative to
#' the \code{@window} slot, or absolute values.
#' @param geom [\code{geom}]\cr Object of class \code{\link{geom}}.
#' @param range [\code{list(2)}]\cr integerish vector of length two for \code{x}
#'   and \code{y}.
#' @param to [\code{character(1)}]\cr the scale to which the coordinates should
#'   be transformed; possible are \code{"relative"} and \code{"absolute"};
#'   ignored in case \code{range != NULL}.
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
#' (relCoords <- gt_scale(geom = aGeom, to = "relative"))
#' gt_scale(geom = relCoords, to = "absolute")
#'
#' gt_scale(geom = aGeom, range = list(x = c(0, 100), y = c(10, 90)))
#'
#' @importFrom checkmate testList assertNames
#' @importFrom methods new
#' @export

gt_scale <- function(geom, range = NULL, to = "relative"){

  assertClass(geom, classes = "geom")
  existsRange <- testTRUE(!is.null(range))
  if(existsRange){
    assertList(range, len = 2, names = "named")
    assertNames(names(range), permutation.of = c("x", "y"))
    assertIntegerish(range$x, len = 2, any.missing = FALSE)
    assertIntegerish(range$y, len = 2, any.missing = FALSE)
    to <- "relative"
  } else{
    to <- assertChoice(x = to, choices = c("relative", "absolute"))
  }

  vert <- geom@vert
  window <- geom@window

  out <- NULL
  if(to == "relative"){
    if(existsRange){
      newScale <- "an 'absolute'"
      rangeX <- range$x
      rangeY <- range$y
    } else{
      newScale <- "a 'relative'"
      rangeX <- c(0, 1)
      rangeY <- c(0, 1)
    }
    minX <- min(window$x)
    maxX <- max(window$x)
    minY <- min(window$y)
    maxY <- max(window$y)
  } else{
    newScale <- "an 'absolute'"
    rangeX <- c(min(window$x), max(window$x))
    rangeY <- c(min(window$y), max(window$y))
    minX <- 0
    maxX <- 1
    minY <- 0
    maxY <- 1
  }

  temp <- vert
  temp$x <- (temp$x - minX) * (rangeX[2] - rangeX[1]) / (maxX - minX) + rangeX[1]
  temp$y <- (temp$y - minY) * (rangeY[2] - rangeY[1]) / (maxY - minY) + rangeY[1]
  out <- rbind(out, temp)

  if(existsRange){
    window <- as.data.frame(range)
  }

  out <- new(Class = "geom",
             type = geom@type,
             vert = out,
             feat = geom@feat,
             group = geom@group,
             window = window,
             scale = to,
             crs = geom@crs,
             history = c(geom@history, list(paste0("vertex values were scaled to ", newScale, " scale."))))

  return(out)
}