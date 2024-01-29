#' Reflect geometric objects
#'
#' Reflect geometric objects across a reflection axis.
#' @param obj [gridded(1)][geom]\cr the object to reflect.
#' @param angle [numeric(1)][numeric]\cr the counter-clockwise angle by which the
#'   reflection axis shall be rotated (can be negative to rotate clockwise).
#' @param fid [integerish(1)][integer]\cr in case only a subset of features shall
#'   be rotated, specify that here.
#' @param update [logical(1)][logical]\cr whether or not to update the window slot
#'   after rotation.
#' @details The reflection axis is a straight line that goes through the plot
#'   origin with the given angle, where positive angles open towards the
#'   positive y-axis and negative angles open up towards the negative y-axis.
#' @return \code{geom} of the reflected \code{obj}.
#' @family geometry tools
#' @examples
#' # reflect several features
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_reflect(obj = gtGeoms$polygon, angle = 45,
#'                        update = FALSE)
#' geo_vis(newPoly, linecol = "green", new = FALSE)
#'
#' # reflect a single feature
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_reflect(obj = gtGeoms$polygon, angle = 90, fid = 2,
#'                        update = FALSE)
#' geo_vis(newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertIntegerish assertLogical
#' @importFrom geomio getFeatures getGroups getPoints getWindow getNames getType
#'   getCRS getProvenance
#' @importFrom dplyr filter bind_rows
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

geo_reflect <- function(obj, angle, fid = NULL, update = TRUE){

  assertNumeric(x = angle, any.missing = FALSE, lower = -360, upper = 360, min.len = 1)
  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  thePoints <- getPoints(x = obj)

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
    tempCoords <- filter(thePoints, fid == ids[i])
    newCoords <- tempCoords

    if(doReflect[i]){
      tempAngle <- angle[[i]]

      newCoords$x <- round(tempCoords$x * cos(2 * .rad(tempAngle)) + tempCoords$y * sin(2 * .rad(tempAngle)), digits)
      newCoords$y <- round(tempCoords$x * sin(2 * .rad(tempAngle)) - tempCoords$y * cos(2 * .rad(tempAngle)), digits)
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
    hist <- paste0("geom was reflected.")
  } else {
    hist <- paste0("geoms were reflected.")
  }

  tempData <- list(features = getFeatures(x = obj), groups = getGroups(x = obj))
  theData <- stats::setNames(list(tempData), getNames(x = obj))

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = obj)[1],
             geometry = temp,
             data = theData,
             window = window,
             crs = getCRS(x = obj),
             provenance = c(getProvenance(x = obj), list(hist)))

  return(out)
}
