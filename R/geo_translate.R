#' Translate geometric objects
#'
#' Translate geometric objects by adding a constant in x and y dimension.
#' @param obj [gridded(1)][geom]\cr the object to translate.
#' @param x [numeric(1)][numeric]\cr the translation constant (offset) in x
#'   dimension.
#' @param y [numeric(1)][numeric]\cr the translation constant (offset) in y
#'   dimension.
#' @param fid [integerish(.)][integer]\cr in case only a subset of features shall
#'   be translated, specify that here.
#' @param update [logical(1)][logical]\cr whether or not to update the window slot
#'   after translation.
#' @return \code{geom} of the mathematically translated \code{obj}.
#' @family geometry tools
#' @examples
#' # translate several features
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_translate(obj = gtGeoms$polygon, x = 5, y = c(-10, 5),
#'                         update = FALSE)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # translate a single feature
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_translate(obj = gtGeoms$polygon, x = 5, fid = 2,
#'                         update = FALSE)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertLogical
#' @importFrom geomio getFeatures getGroups getPoints getWindow getNames getType
#'   getCRS getProvenance
#' @importFrom dplyr filter bind_rows
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

geo_translate <- function(obj, x = NULL, y = NULL, fid = NULL, update = TRUE){

  assertNumeric(x, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(y, any.missing = FALSE, min.len = 1, null.ok = TRUE)
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
    tempCoords <- filter(thePoints, fid == ids[i])
    newCoords <- tempCoords

    if(doTranslate[i]){
      newCoords$x <- tempCoords$x + x[[i]]
      newCoords$y <- tempCoords$y + y[[i]]
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
    hist <- paste0("geom was translated.")
  } else {
    hist <- paste0("geoms were translated.")
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