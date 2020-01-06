#' Scale \code{geom}s
#'
#' Scale the vertex values of \code{geom}s to a values range or so that they are
#' either relative to the \code{@window} slot, or absolute values.
#' @param geom [\code{geom(.)}]\cr the object to be scaled.
#' @param range [\code{list(2)}]\cr vector of length two for both of the
#'   \code{x} and \code{y} dimension to which the values should be scaled.
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
#' # scale to another range
#' gt_scale(geom = aGeom, range = list(x = c(0, 100), y = c(10, 90)))
#'
#' @importFrom checkmate testList assertNames assertChoice
#' @importFrom tibble as_tibble
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

  point <- getPoints(x = geom)
  window <- getWindow(x = geom)

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

    if(minX == maxX){
      stop("I can't scale a 'geom' that has equal minimum and maximum x values of the window.")
    }
    if(minY == maxY){
      stop("I can't scale a 'geom' that has equal minimum and maximum y values of the window.")
    }
  } else{
    newScale <- "an 'absolute'"
    rangeX <- c(min(window$x), max(window$x))
    rangeY <- c(min(window$y), max(window$y))
    minX <- 0
    maxX <- 1
    minY <- 0
    maxY <- 1
  }

  temp <- point
  temp$x <- (temp$x - minX) * (rangeX[2] - rangeX[1]) / (maxX - minX) + rangeX[1]
  temp$y <- (temp$y - minY) * (rangeY[2] - rangeY[1]) / (maxY - minY) + rangeY[1]
  out <- rbind(out, temp)

  if(existsRange){
    window <- as.data.frame(range)
  }

  if(geom@type == "grid"){
    theFeatures <- getFeatures(x = geom)
    theGroups <- getGroups(x = geom)
  } else {
    theFeatures <- list(geometry = getFeatures(x = geom))
    theGroups <- list(geometry = getGroups(x = geom))
  }

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = geom)[2],
             point = as_tibble(out),
             feature = theFeatures,
             group = theGroups,
             window = window,
             scale = to,
             crs = getCRS(x = geom),
             history = c(getHistory(x = geom)))

  # assign history
  out <- setHistory(x = out, history = paste0("vertex values were scaled to ", newScale, " scale."))

  return(out)
}