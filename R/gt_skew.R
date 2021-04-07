#' Skew geometric objects
#'
#' Skew geometric objects by a shear factor in x and y dimension.
#' @param obj [\code{geometric object(1)}]\cr the object to skew.
#' @param x [\code{numeric(1)}]\cr the shear factor in x dimension.
#' @param y [\code{numeric(1)}]\cr the shear factor in y dimension.
#' @param fid [\code{integerish(.)}]\cr in case only a subset of features shall
#'   be skewed, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after skewing.
#' @return \code{geom} of the skewed \code{obj}.
#' @family geometry tools
#' @examples
#' # skew several features
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_skew(obj = gtGeoms$polygon, x = 0.5, update = FALSE)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # skew a single feature
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_skew(obj = gtGeoms$polygon, x = 0.5, y = .7, fid = 2,
#'                    update = FALSE)
#' visualise(newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertIntegerish assertLogical
#' @importFrom tibble tibble as_tibble
#' @importFrom methods new
#' @export

gt_skew <- function(obj, x = NULL, y = NULL, fid = NULL, update = TRUE){

  assertNumeric(x = x, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(x = y, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertIntegerish(x = fid, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  theFeatures <- getFeatures(x = obj)
  theGroups <- getGroups(x = obj)
  thePoints <- getPoints(x = obj)
  thewindow <- getWindow(x = obj)

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
    doSkew <- ids %in% fid
  } else{
    doSkew <- rep(TRUE, length(ids))
  }

  # repeat values to match fids
  if(length(x) != length(ids)){
    x <- rep(x, length.out = length(ids))
  }
  if(length(y) != length(ids)){
    y <- rep(y, length.out = length(ids))
  }

  # modify vertices
  temp <- NULL
  for(i in seq_along(ids)){
    tempCoords <- thePoints[thePoints$fid == ids[i],]
    newCoords <- tempCoords

    if(doSkew[i]){
      centCoords <- tempCoords[!duplicated(tempCoords),]
      centroid <- tibble(x = mean(x = centCoords$x), y = mean(x = centCoords$y))

      newCoords$x <- tempCoords$x + x[[i]] * tempCoords$y
      newCoords$y <- tempCoords$y + y[[i]] * tempCoords$x

      centCoords <- newCoords[!duplicated(newCoords),]
      newCentroid <- tibble(x = mean(x = centCoords$x), y = mean(x = centCoords$y))
      offset <- newCentroid - centroid

      newCoords$x <- newCoords$x - offset$x
      newCoords$y <- newCoords$y - offset$y
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
    hist <- paste0("geom was skewed.")
  } else {
    hist <- paste0("geoms were skewed.")
  }

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = obj)[1],
             point = as_tibble(temp),
             feature = theFeatures,
             group = theGroups,
             window = window,
             crs = getCRS(x = obj),
             history = c(getHistory(x = obj), list(hist)))

  return(out)
}