#' Scale geometric objects
#'
#' Scale the vertex values of geometric objects to a values range.
#' @param obj [\code{geometric object(1)}]\cr the object to be scaled.
#' @param range [\code{data.frame(2)}]\cr vector of length two for both of the
#'   \code{x} and \code{y} dimension to which the values should be scaled.
#' @param fid [\code{integerish(.)}]\cr if only a subset of features shall be
#'   scaled, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after scaling.
#' @return \code{geom} of the scaled \code{obj}.
#' @family geometry tools
#' @examples
#' # rescale to values between -10 and 10
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_scale(obj = gtGeoms$polygon, update = FALSE,
#'                     range = data.frame(x = c(-10, 10), y = c(-10, 10)))
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # rescale a single feature
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_scale(obj = gtGeoms$polygon, update = FALSE, fid = 2,
#'                     range = data.frame(x = c(-10, 10), y = c(-10, 10)))
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate testList assertNames assertChoice
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @export

gt_scale <- function(obj, range = NULL, fid = NULL, update = TRUE){

  assertDataFrame(x = range, types = "numeric", any.missing = FALSE, ncols = 2)
  assertNames(names(range), permutation.of = c("x", "y"))
  assertLogical(x = update, len = 1, any.missing = FALSE)

  thePoints <- getPoints(x = obj)
  theFeatures <- getFeatures(x = obj)
  theGroups <- getGroups(x = obj)
  theType <- getType(x = obj)[1]
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
    window <- .updateWindow(input = temp, window = theWindow)
  } else {
    window <- theWindow
  }

  # make history
  hist <- paste0("coordinate values were rescaled between x[", paste0(range$x, collapse = " "), "] and y[",  paste0(range$y, collapse = " "), "]")

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = obj)[1],
             point = temp,
             feature = theFeatures,
             group = theGroups,
             window = window,
             crs = getCRS(x = obj),
             history =c(getHistory(x = obj), list(hist)))

  return(out)
}