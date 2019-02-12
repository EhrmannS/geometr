#' Scale geometries
#'
#' Relative coordinates are required to work with grobs of \code{geom}s, absolute
#' coordinates are required to create spatial objects thereof.
#' @template geom
#' @param range [\code{list(2)}]\cr integerish vector of length two for \code{x}
#'   and \code{y}.
#' @param to [\code{character(1)}]\cr the scale to which the coordinates should
#'   be transformed; possible are \code{"relative"} and \code{"absolute"};
#'   ignored in case \code{range != NULL}.
#' @return Scaled \code{geom}.
#' @examples
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70),
#'                      fid = 1)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window, col = "blue")
#'
#' # change to relative scale and back to absolute
#' (relCoords <- gScale(geom = aGeom, to = "relative"))
#' gScale(geom = relCoords, to = "absolute")
#'
#' gScale(geom = aGeom, range = list(x = c(0, 100), y = c(10, 90)))
#'
#' @importFrom checkmate testList assertNames
#' @importFrom methods new
#' @export

gScale <- function(geom, range = NULL, to = "relative"){

  assertClass(geom, classes = "geom")
  existsRange <- testTRUE(!is.null(range))
  if(existsRange){
    assertList(range, len = 2, names = "named")
    assertNames(names(range), permutation.of = c("x", "y"))
    assertIntegerish(range$x, len = 2, any.missing = FALSE)
    assertIntegerish(range$y, len = 2, any.missing = FALSE)
    to <- "relative"
  } else{
    to <- match.arg(to, c("relative", "absolute"))
  }

  coords <- geom@coords
  window <- geom@window

  out <- NULL
  if(to == "relative"){
    if(existsRange){
      rangeX <- range$x
      rangeY <- range$y
    } else{
      rangeX <- c(0, 1)
      rangeY <- c(0, 1)
    }
    minX <- min(window$x)
    maxX <- max(window$x)
    minY <- min(window$y)
    maxY <- max(window$y)
  } else{
    rangeX <- c(min(window$x), max(window$x))
    rangeY <- c(min(window$y), max(window$y))
    minX <- 0
    maxX <- 1
    minY <- 0
    maxY <- 1
  }

  temp <- coords
  temp$x <- (temp$x - minX) * (rangeX[2] - rangeX[1]) / (maxX - minX) + rangeX[1]
  temp$y <- (temp$y - minY) * (rangeY[2] - rangeY[1]) / (maxY - minY) + rangeY[1]
  out <- rbind(out, temp)

  if(existsRange){
    window <- as.data.frame(range)
    to <- "absolute"
  }
  out <- new(Class = "geom",
             type = geom@type,
             coords = out,
             attr = geom@attr,
             window = window,
             scale = to,
             crs = geom@crs,
             history = list(paste0("geometry values were scaled to '", to, "'")))

  return(out)
}