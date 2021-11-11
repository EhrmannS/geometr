#' Reflect geometric objects
#'
#' Reflect geometric objects across a reflection axis.
#' @param obj [\code{geometric object(1)}]\cr the object to reflect.
#' @param angle [\code{numeric(1)}]\cr the counter-clockwise angle by which the
#'   reflection axis shall be rotated (can be negative to rotate clockwise).
#' @param fid [\code{integerish(.)}]\cr in case only a subset of features shall
#'   be rotated, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after rotation.
#' @details The reflection axis is a straight line that goes through the plot
#'   origin with the given angle, where positive angles open towards the
#'   positive y-axis and negative angles open up towards the negative y-axis.
#' @return \code{geom} of the reflected \code{obj}.
#' @family geometry tools
#' @examples
#' # reflect several features
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_reflect(obj = gtGeoms$polygon, angle = 45,
#'                       update = FALSE)
#' visualise(newPoly, linecol = "green", new = FALSE)
#'
#' # reflect a single feature
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_reflect(obj = gtGeoms$polygon, angle = 90, fid = 2,
#'                       update = FALSE)
#' visualise(newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertIntegerish assertLogical
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

gt_reflect <- function(obj, angle, fid = NULL, update = TRUE){

  assertNumeric(x = angle, any.missing = FALSE, lower = -360, upper = 360, min.len = 1)
  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  theFeatures <- getFeatures(x = obj)
  theGroups <- getGroups(x = obj)
  thePoints <- getPoints(x = obj)
  theWindow <- getWindow(x = obj)
  theName <- getNames(x = obj)

  # identify fids to modify
  ids <- unique(thePoints$fid)
  existsID <- !is.null(fid)
  if(existsID){
    doReflect <- ids %in% fid
  } else{
    doReflect <- rep(TRUE, length(ids))
  }

  # repeat values to match fids
  if(length(angle) != length(ids)){
    angle <- rep(angle, length.out = length(ids))
  }

  digits <- getOption("digits")

  # modify vertices
  temp <- NULL
  for(i in seq_along(ids)){
    tempCoords <- thePoints[thePoints$fid == ids[i],]
    newCoords <- tempCoords

    if(doReflect[i]){
      tempAngle <- angle[[i]]

      newCoords$x <- round(tempCoords$x * cos(2 * .rad(tempAngle)) + tempCoords$y * sin(2 * .rad(tempAngle)), digits)
      newCoords$y <- round(tempCoords$x * sin(2 * .rad(tempAngle)) - tempCoords$y * cos(2 * .rad(tempAngle)), digits)
    }
    temp <- rbind(temp, newCoords)
  }

  # update window
  if(update){
    window <- .updateWindow(input = temp, window = theWindow)
  } else {
    window <- theWindow
  }

  # make history
  if(length(ids) == 1){
    hist <- paste0("geom was reflected.")
  } else {
    hist <- paste0("geoms were reflected.")
  }

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = obj)[1],
             name = theName,
             point = temp,
             feature = theFeatures,
             group = theGroups,
             window = window,
             crs = getCRS(x = obj),
             history = c(getHistory(x = obj), list(hist)))

  return(out)
}
