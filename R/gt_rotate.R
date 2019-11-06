#' Rotate \code{geom}s
#'
#' Rotate \code{geom}s by a certain angle about a center
#' @param geom [\code{geom(.)}]\cr the object to rotate.
#' @param angle [\code{numeric(1)}]\cr the counter-clockwise angle by which
#'   \code{geom} shall be rotated (can be negative to rotate clockwise).
#' @param about [\code{numeric(2)}]\cr the point about which \code{geom} shall
#'   be rotated.
#' @param fid [\code{integerish(.)}]\cr if only a subset of features shall be
#'   rotated, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after rotation.
#' @return Rotated \code{geom}.
#' @family geometry tools
#' @examples
#' # the original object
#' coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
#'                      y = c(40, 40, 60, 70, 10, 20, 40),
#'                      fid = c(1, 1, 1, 1, 2, 2, 2))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#' visualise(geom = aGeom)
#'
#' # rotate all geoms
#' rotatedGeom <- gt_rotate(geom = aGeom, angle = 90, about = c(40, 40))
#' visualise(geom = rotatedGeom)
#'
#' # rotate a single geom
#' rotTri <- gt_rotate(geom = aGeom, angle = -180, about = c(30, 40), fid = 2)
#' visualise(geom = rotTri)
#'
#' # rotate different geoms about different centers by different angles
#' rotateMore <- gt_rotate(geom = aGeom,
#'                         angle = list(90, -180),
#'                         about = list(c(40, 40), c(30, 40)))
#' visualise(geom = rotateMore)
#' @importFrom checkmate assertClass assertNames testList testNumeric
#'   assertNumeric
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @export

gt_rotate <- function(geom = NULL, angle = NULL, about = c(0, 0), fid = NULL,
                      update = TRUE){

  assertClass(geom, classes = "geom")
  angleIsList <- testList(angle, types = "numeric", any.missing = FALSE)
  angleIsNumeric <- testNumeric(angle, any.missing = FALSE, lower = -360, upper = 360, len = 1)
  aboutIsList <- testList(about, types = "numeric", any.missing = FALSE)
  aboutIsNumeric <- testNumeric(about, any.missing = FALSE, len = 2)
  assert(angleIsList, angleIsNumeric)
  assert(aboutIsList, aboutIsNumeric)
  assertIntegerish(x = fid, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  theFeatures <- getFeatures(x = geom)
  theGroups <- getGroups(x = geom)

  # make list, if it is not yet
  if(aboutIsNumeric){
    about <- list(about)
  }
  if(angleIsNumeric){
    angle <- list(angle)
  }

  verts <- getPoints(x = geom)
  thewindow <- getWindow(x = geom)
  ids <- unique(verts$fid)

  # identify fids to modify
  existsID <- !is.null(fid)
  if(existsID){
    doRotate <- ids %in% fid
  } else{
    doRotate <- rep(TRUE, length(ids))
  }

  # repeat values to match fids
  if(length(angle) != length(ids)){
    angle <- rep(angle, length.out = length(ids))
  }
  if(length(about) != length(ids)){
    about <- rep(about, length.out = length(ids))
  }

  digits <- getOption("digits")

  # modify vertices
  temp <- NULL
  for(i in seq_along(ids)){
    tempCoords <- verts[verts$fid == ids[i],]
    newCoords <- tempCoords

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

      newCoords$x <- round(xVals * cos(.rad(tempAngle)) - yVals * sin(.rad(tempAngle)), digits)
      newCoords$y <- round(xVals * sin(.rad(tempAngle)) + yVals * cos(.rad(tempAngle)), digits)

      if(!all(tempAbout == c(0, 0))){
        newCoords$x <- tempCoords$x - offset[1]
        newCoords$y <- tempCoords$y - offset[2]
      }
    }
    temp <- rbind(temp, newCoords)
  }

  # update window
  if(update){
    window <- .updateWindow(input = temp, window = thewindow)
  } else {
    window <- thewindow
  }

  # make history
  if(length(ids) == 1){
    hist <- paste0("geom was rotated.")
  } else {
    hist <- paste0("geoms were rotated.")
  }

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = geom)[2],
             point = as_tibble(temp),
             feature = list(geometry = theFeatures),
             group = list(geometry = theGroups),
             window = window,
             scale = geom@scale,
             crs = getCRS(x = geom),
             history = c(getHistory(x = geom), list(hist)))

  return(out)
}