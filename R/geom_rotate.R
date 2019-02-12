#' Rotate geometries
#'
#' Rotate \code{geom}s by a certain angle about a center
#' @template geom
#' @param angle [\code{numeric(1)}]\cr the counter-clockwise angle by which
#'   \code{geom} shall be rotated.
#' @param about [\code{numeric(2)}]\cr the point about which \code{geom} shall
#'   be rotated.
#' @param fid [\code{integerish(.)}]\cr vector of features that should be
#'   rotated.
#' @return Rotated \code{geom}.
#' @examples
#' coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
#'                      y = c(40, 40, 60, 70, 10, 20, 40),
#'                      fid = c(1, 1, 1, 1, 2, 2, 2))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window, show = TRUE)
#'
#' rotatedGeom <- gRotate(geom = aGeom, angle = 90, about = c(40, 40))
#' visualise(geom = rotatedGeom)
#'
#' # rotate single objects
#' rotatedTriangle <- gRotate(geom = aGeom, angle = -180, about = c(30, 40), fid = 2)
#' visualise(geom = rotatedTriangle, col = "goldenrod1")
#'
#' # rotate different objects about different centers by different angles
#' rotateMore <- gRotate(geom = aGeom,
#'                       angle = list(90, -180),
#'                       about = list(c(40, 40), c(30, 40)))
#' visualise(geom = rotateMore, col = "deeppink")
#' @importFrom checkmate assertNames testList testNumeric assertNumeric
#' @importFrom methods new
#' @export

gRotate <- function(geom, angle, about = c(0, 0), fid = NULL){

  assertClass(geom, classes = "geom")
  angleIsList <- testList(angle, types = "numeric", any.missing = FALSE)
  angleIsNumeric <- testNumeric(angle, any.missing = FALSE, lower = -360, upper = 360, len = 1)
  aboutIsList <- testList(about, types = "numeric", any.missing = FALSE)
  aboutIsNumeric <- testNumeric(about, any.missing = FALSE, len = 2)
  assert(angleIsList, angleIsNumeric)
  assert(aboutIsList, aboutIsNumeric)
  if(aboutIsNumeric){
    about <- list(about)
  }
  if(angleIsNumeric){
    angle <- list(angle)
  }
  existsID <- !is.null(fid)

  coords <- geom@coords
  ids <- unique(coords$fid)
  if(existsID){
    doRotate <- ids %in% fid
  } else{
    doRotate <- rep(TRUE, length(ids))
  }

  if(length(angle) != length(ids)){
    angle <- rep(angle, length.out = length(ids))
  }
  if(length(about) != length(ids)){
    about <- rep(about, length.out = length(ids))
  }

  digits <- getOption("digits")

  # out <- geom
  temp <- NULL
  for(i in seq_along(ids)){
    tempCoords <- coords[coords$fid == ids[i],]

    if(doRotate[i]){
      tempAngle <- angle[[i]]
      tempAbout <- about[[i]]
      xVals <- tempCoords$x
      yVals <- tempCoords$y

      if(!all(tempAbout == c(0, 0))){
        offset <- c(0, 0) - tempAbout
        xVals <- xVals + offset[1]
        yVals <- yVals + offset[2]
      }

      tempCoords$x <- round(xVals * cos(rad(tempAngle)) - yVals * sin(rad(tempAngle)), digits)
      tempCoords$y <- round(xVals * sin(rad(tempAngle)) + yVals * cos(rad(tempAngle)), digits)

      if(!all(tempAbout == c(0, 0))){
        tempCoords$x <- tempCoords$x - offset[1]
        tempCoords$y <- tempCoords$y - offset[2]
      }
    }
    temp <- rbind(temp, tempCoords)

  }
  out <- new(Class = "geom",
             type = geom@type,
             coords = temp,
             attr = geom@attr,
             window = geom@window,
             scale = geom@scale,
             crs = geom@crs,
             history = list(paste0("geometry was rotated")))

  return(out)
}