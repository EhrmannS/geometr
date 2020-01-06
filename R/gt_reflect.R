#' Reflect \code{geom}s
#'
#' Reflect \code{geom}s across a reflection axis.
#' @param geom [\code{geom(.)}]\cr the object to reflect.
#' @param angle [\code{numeric(1)}]\cr the counter-clockwise angle by which the
#'   reflection axis shall be rotated (can be negative to rotate clockwise).
#' @param fid [\code{integerish(.)}]\cr if only a subset of features shall be
#'   rotated, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after rotation.
#' @details The reflection axis is a straight line that goes through the plot
#'   origin with the given angle, where positive angles open towards the
#'   positive y-axis and negative angles open up towards the negative y-axis.
#' @examples
#' # the original object
#' coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
#'                      y = c(40, 40, 60, 70, 10, 20, 40),
#'                      fid = c(1, 1, 1, 1, 2, 2, 2))
#' window <- data.frame(x = c(-80, 80),
#'                      y = c(-80, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#'
#' # reflect several geoms
#' visualise(geom = gt_reflect(geom = aGeom, angle = 30))
#'
#' # reflect a single geom
#' visualise(geom = gt_reflect(geom = aGeom, angle = -45, fid = 1))
#' @return Reflected \code{geom}.
#' @family geometry tools
#' @importFrom checkmate assertClass testList testNumeric assert
#'   assertIntegerish assertLogical
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

gt_reflect <- function(geom = NULL, angle = NULL, fid = NULL, update = TRUE){

  assertClass(geom, classes = "geom")
  angleIsList <- testList(angle, types = "numeric", any.missing = FALSE)
  angleIsNumeric <- testNumeric(angle, any.missing = FALSE, lower = -360, upper = 360, len = 1)
  assert(angleIsList, angleIsNumeric)
  assertIntegerish(x = fid, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  theFeatures <- getFeatures(x = geom)
  theGroups <- getGroups(x = geom)

  # make list, if it is not yet
  if(angleIsNumeric){
    angle <- list(angle)
  }

  verts <- getPoints(x = geom)
  ids <- unique(verts$fid)

  # identify fids to modify
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
    tempCoords <- verts[verts$fid == ids[i],]
    newCoords <- tempCoords

    if(doReflect[i]){
      tempAngle <- angle[[i]]

      newCoords$x <- round(tempCoords$x * cos(2 * .rad(tempAngle)) + tempCoords$y * sin(2 * .rad(tempAngle)), digits)
      newCoords$y <- round(tempCoords$x * sin(2 * .rad(tempAngle)) - tempCoords$y * cos(2 * .rad(tempAngle)), digits)
    }
    temp <- rbind(temp, newCoords)
  }

  # update window
  thewindow <- getWindow(x = geom)
  if(update){
    window <- .updateWindow(input = temp, window = thewindow)
  } else {
    window <- thewindow
  }

  # make history
  if(length(ids) == 1){
    hist <- paste0("geom was reflected.")
  } else {
    hist <- paste0("geoms were reflected.")
  }

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = geom)[2],
             point = temp,
             feature = list(geometry = theFeatures),
             group = list(geometry = theGroups),
             window = window,
             scale = geom@scale,
             crs = getCRS(x = geom),
             history = c(getHistory(x = geom), list(hist)))

  return(out)
}
