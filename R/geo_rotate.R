#' Rotate geometric objects
#'
#' Rotate geometric objects by a certain angle about center coordinates
#' @param obj [gridded(1)][geom]\cr the object to rotate.
#' @param x [numeric(1)][numeric]\cr the x position(s) to rotate about.
#' @param y [numeric(1)][numeric]\cr the y position(s) to rotate about.
#' @param angle [numeric(1)][numeric]\cr the counter-clockwise angle(s) by which
#'   \code{obj} shall be rotated (can be negative to rotate clockwise).
#' @param fid [integerish(.)][integer]\cr in case only a subset of features shall
#'   be rotated, specify that here.
#' @param update [logical(1)][logical]\cr whether or not to update the window slot
#'   of the resulting geom.
#' @return \code{geom} of the rotated \code{obj}.
#' @family geometry tools
#' @examples
#' # rotate all geoms
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_rotate(obj = gtGeoms$polygon, x = 0, y = 0, angle = 135,
#'                       update = FALSE)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # rotate a single geom
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_rotate(obj = gtGeoms$polygon, x = -10, y = 0, angle = -180,
#'                       update = FALSE, fid = 2)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # rotate different geoms about different centers by different angles
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_rotate(obj = gtGeoms$polygon,
#'                       x = c(0, -10),
#'                       y = c(-10, 0),
#'                       angle = c(75, -135),
#'                       update = FALSE)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertLogical
#' @importFrom geomio getFeatures getGroups getPoints getWindow getNames getType
#'   getCRS getProvenance
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @export

geo_rotate <- function(obj, x = NULL, y = NULL, angle = NULL, fid = NULL,
                      update = TRUE){

  assertNumeric(x = x, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(x = y, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(angle, any.missing = FALSE, lower = -360, upper = 360, min.len = 1)
  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  thePoints <- getPoints(x = obj)

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
    tempCoords <- filter(thePoints, fid == ids[i])
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
    temp <- bind_rows(temp, newCoords)
  }

  # update window
  if(update){
    window <- tibble(x = c(min(temp$x), max(temp$x)),
                     y = c(min(temp$y), max(temp$y)))
  } else {
    window <- getWindow(x = obj)
  }

  # make history
  if(length(ids) == 1){
    hist <- paste0("geom was rotated.")
  } else {
    hist <- paste0("geoms were rotated.")
  }

  # make new geom
  tempData <- list(features = getFeatures(x = obj), groups = getGroups(x = obj))
  theData <- stats::setNames(list(tempData), getNames(x = obj))

  out <- new(Class = "geom",
             type = getType(x = obj)[1],
             geometry = temp,
             data = theData,
             window = window,
             crs = getCRS(x = obj),
             provenance = c(getProvenance(x = obj), list(hist)))

  return(out)
}