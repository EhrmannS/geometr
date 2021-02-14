#' Rotate geometric objects
#'
#' Rotate geometric objects by a certain angle about center coordinates
#' @param obj [\code{geometric object(1)}]\cr the object to rotate.
#' @param x [\code{numeric(1)}]\cr the x position(s) to rotate about.
#' @param y [\code{numeric(1)}]\cr the y position(s) to rotate about.
#' @param angle [\code{numeric(1)}]\cr the counter-clockwise angle by which
#'   \code{geom} shall be rotated (can be negative to rotate clockwise).
#' @param fid [\code{integerish(.)}]\cr if only a subset of features shall be
#'   rotated, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after rotation.
#' @return \code{geom} of the rotated \code{obj}.
#' @family geometry tools
#' @examples
#' # rotate all geoms
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_rotate(obj = gtGeoms$polygon, x = 0, y = 0, angle = 135,
#'                      update = FALSE)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # rotate a single geom
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_rotate(obj = gtGeoms$polygon, x = -10, y = 0, angle = -180,
#'                      update = FALSE, fid = 2)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # rotate different geoms about different centers by different angles
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_rotate(obj = gtGeoms$polygon,
#'                      x = c(0, -10),
#'                      y = c(-10, 0),
#'                      angle = c(75, -135),
#'                      update = FALSE)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertIntegerish assertLogical
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @export

gt_rotate <- function(obj, x = NULL, y = NULL, angle = NULL, fid = NULL,
                      update = TRUE){

  assertNumeric(x = x, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(x = y, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(angle, any.missing = FALSE, lower = -360, upper = 360, min.len = 1)
  assertIntegerish(x = fid, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  theFeatures <- getFeatures(x = obj)
  theGroups <- getGroups(x = obj)
  thePoints <- getPoints(x = obj)
  theWindow <- getWindow(x = obj)

  # set default values
  if(is.null(x)){
    x <- 0
  }
  if(is.null(y)){
    y <- 0
  }
  ids <- unique(thePoints$fid)

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
  if(length(x) != length(ids)){
    x <- rep(x, length.out = length(ids))
  }
  if(length(y) != length(ids)){
    y <- rep(y, length.out = length(ids))
  }

  digits <- getOption("digits")

  # modify vertices
  temp <- NULL
  for(i in seq_along(ids)){
    tempCoords <- thePoints[thePoints$fid == ids[i],]
    newCoords <- tempCoords

    if(doRotate[i]){
      tempAngle <- angle[i]
      tempX <- x[i]
      tempY <- y[i]
      xVals <- tempCoords$x
      yVals <- tempCoords$y

      # offset
      xVals <- xVals + (0 - tempX)
      yVals <- yVals + (0 - tempY)

      newCoords$x <- round(xVals * cos(.rad(tempAngle)) - yVals * sin(.rad(tempAngle)), digits)
      newCoords$y <- round(xVals * sin(.rad(tempAngle)) + yVals * cos(.rad(tempAngle)), digits)

      # undo offset
      newCoords$x <- newCoords$x - (0 - tempX)
      newCoords$y <- newCoords$y - (0 - tempY)
    }
    temp <- rbind(temp, newCoords)
  }

  # update window
  if(update){
    theWindow <- .updateWindow(input = temp, window = theWindow)
  }

  # make history
  if(length(ids) == 1){
    hist <- paste0("geom was rotated.")
  } else {
    hist <- paste0("geoms were rotated.")
  }

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = obj)[1],
             point = as_tibble(temp),
             feature = list(geometry = theFeatures),
             group = list(geometry = theGroups),
             window = theWindow,
             crs = getCRS(x = obj),
             history = c(getHistory(x = obj), list(hist)))

  return(out)
}