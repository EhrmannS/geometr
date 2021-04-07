#' Stretch geometric objects
#'
#' Stretch geometric objects by a scale factor in x and y dimension.
#' @param obj [\code{geometric object(1)}]\cr the object to stretch.
#' @param x [\code{numeric(1)}]\cr the scale factor in x dimension.
#' @param y [\code{numeric(1)}]\cr the scale factor in y dimension.
#' @param fid [\code{integerish(.)}]\cr in case only a subset of features shall
#'   be stretched, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after stretching.
#' @return \code{geom} of the stretched \code{obj}.
#' @family geometry tools
#' @examples
#' # stretch several features
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_stretch(obj = gtGeoms$polygon, x = 0.5, y = 0.2,
#'                       update = FALSE)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # stretch a single feature
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_stretch(obj = gtGeoms$polygon, x = 0.5, fid = 2, update = FALSE)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # stretch feature separately
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_stretch(obj = gtGeoms$polygon,
#'                       x = c(0.2, 1),
#'                       y = c(1, 0.2),
#'                       update = FALSE)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertIntegerish assertLogical
#' @importFrom tibble tibble as_tibble
#' @importFrom methods new
#' @export

gt_stretch <- function(obj, x = NULL, y = NULL, fid = NULL, update = TRUE){

  assertNumeric(x, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(y, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  theFeatures <- getFeatures(x = obj)
  theGroups <- getGroups(x = obj)
  verts <- getPoints(x = obj)
  thewindow <- getWindow(x = obj)

  # set default values
  if(is.null(x)){
    x <- 1
  }
  if(is.null(y)){
    y <- 1
  }

  ids <- unique(verts$fid)

  # identify fids to modify
  existsID <- !is.null(fid)
  if(existsID){
    doStretch <- ids %in% fid
  } else{
    doStretch <- rep(TRUE, length(ids))
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
    tempCoords <- verts[verts$fid == ids[i],]
    newCoords <- tempCoords

    if(doStretch[i]){
      centCoords <- tempCoords[!duplicated(tempCoords),]
      centroid <- tibble(x = mean(x = centCoords$x), y = mean(x = centCoords$y))

      newCoords$x <- x[[i]] * tempCoords$x
      newCoords$y <- y[[i]] * tempCoords$y

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
    hist <- paste0("geom was stretched.")
  } else {
    hist <- paste0("geoms were stretched.")
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
