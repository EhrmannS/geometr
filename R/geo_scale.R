#' Scale geometric objects
#'
#' Scale the vertex values of geometric objects to a values range.
#' @param obj [gridded(1)][geom]\cr the object to be scaled.
#' @param range [data.frame(2)][data.frame]\cr vector of length two for both of the
#'   \code{x} and \code{y} dimension to which the values should be scaled.
#' @param fid [integerish(.)][integer]\cr in case only a subset of features shall
#'   be scaled, specify that here.
#' @param update [logical(1)][logical]\cr whether or not to update the window slot
#'   of the resulting geom.
#' @return \code{geom} of the scaled \code{obj}.
#' @family geometry tools
#' @examples
#' # rescale to values between -10 and 10
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_scale(obj = gtGeoms$polygon, update = FALSE,
#'                      range = data.frame(x = c(-10, 10), y = c(-10, 10)))
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # rescale a single feature
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_scale(obj = gtGeoms$polygon, update = FALSE, fid = 2,
#'                      range = data.frame(x = c(-10, 10), y = c(-10, 10)))
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertDataFrame assertNames assertNumeric assertLogical
#' @importFrom geomio getPoints getFeatures getGroups getType getWindow getNames
#'   getProvenance
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

geo_scale <- function(obj, range = NULL, fid = NULL, update = TRUE){

  assertDataFrame(x = range, types = "numeric", any.missing = FALSE, ncols = 2)
  assertNames(names(range), permutation.of = c("x", "y"))
  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  thePoints <- getPoints(x = obj)
  theWindow <- getWindow(x = obj)

  # identify fids to modify
  ids <- unique(thePoints$fid)
  existsID <- !is.null(fid)
  if(existsID){
    doScale <- ids %in% fid
  } else{
    doScale <- rep(TRUE, length(ids))
  }

  minX <- min(theWindow$x)
  maxX <- max(theWindow$x)
  minY <- min(theWindow$y)
  maxY <- max(theWindow$y)

  if(minX == maxX){
    stop("I can't scale a 'geom' that has equal minimum and maximum x values of the window.")
  }
  if(minY == maxY){
    stop("I can't scale a 'geom' that has equal minimum and maximum y values of the window.")
  }

  temp <- NULL
  for(i in seq_along(ids)){
    tempCoords <- thePoints[thePoints$fid == ids[i],]
    newCoords <- tempCoords

    if(doScale[i]){
      newCoords$x <- (tempCoords$x - minX) * (max(range$x) - min(range$x)) / (maxX - minX) + min(range$x)
      newCoords$y <- (tempCoords$y - minY) * (max(range$y) - min(range$y)) / (maxY - minY) + min(range$y)
    }

    temp <- rbind(temp, newCoords)
  }

  # update window
  if(update){
    window <- tibble(x = c(min(temp$x), max(temp$x)),
                     y = c(min(temp$y), max(temp$y)))
  } else {
    window <- theWindow
  }

  # make history
  hist <- paste0("coordinate values were rescaled between x[", paste0(range$x, collapse = " "), "] and y[",  paste0(range$y, collapse = " "), "]")

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