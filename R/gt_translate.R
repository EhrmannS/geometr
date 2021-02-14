#' Translate geometric objects
#'
#' Translate geometric objects by adding a constant in x and y dimension.
#' @param obj [\code{geometric object(1)}]\cr the object to translate.
#' @param x [\code{numeric(1)}]\cr the translation constant (offset) in
#'   x dimension.
#' @param y [\code{numeric(1)}]\cr the translation constant (offset) in
#'   y dimension.
#' @param fid [\code{integerish(.)}]\cr if only a subset of features shall be
#'   translated, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after translation.
#' @return \code{geom} of the mathematically translated \code{obj}.
#' @family geometry tools
#' @examples
#' # translate several geoms
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_translate(obj = gtGeoms$polygon, x = 5, y = c(-10, 5),
#'                         update = FALSE)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # translate a single geom
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_translate(obj = gtGeoms$polygon, x = 5, fid = 2,
#'                         update = FALSE)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertClass assertNumeric assertIntegerish
#'   assertLogical
#' @importFrom tibble as_tibble
#' @importFrom methods new
#' @export

gt_translate <- function(obj, x = NULL, y = NULL, fid = NULL, update = TRUE){

  assertNumeric(x, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(y, any.missing = FALSE, min.len = 1, null.ok = TRUE)
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
    doTranslate <- ids %in% fid
  } else{
    doTranslate <- rep(TRUE, length(ids))
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

    if(doTranslate[i]){
      newCoords$x <- tempCoords$x + x[[i]]
      newCoords$y <- tempCoords$y + y[[i]]
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
    hist <- paste0("geom was translated.")
  } else {
    hist <- paste0("geoms were translated.")
  }

  # make new geom
  out <- new(Class = "geom",
             type = getType(x = obj)[1],
             point = as_tibble(temp),
             feature = list(geometry = theFeatures),
             group = list(geometry = theGroups),
             window = window,
             crs = getCRS(x = obj),
             history = c(getHistory(x = obj), list(hist)))

  return(out)
}