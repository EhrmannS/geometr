#' Stretch geometric objects
#'
#' Stretch geometric objects by a scale factor in x and y dimension.
#' @param obj [gridded(1)][geom]\cr the object to stretch.
#' @param x [numeric(1)][numeric]\cr the scale factor in x dimension.
#' @param y [numeric(1)][numeric]\cr the scale factor in y dimension.
#' @param fid [integerish(.)][integer]\cr in case only a subset of features shall
#'   be stretched, specify that here.
#' @param update [logical(1)][logical]\cr whether or not to update the window slot
#'   after stretching.
#' @return \code{geom} of the stretched \code{obj}.
#' @family geometry tools
#' @examples
#' # stretch several features
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_stretch(obj = gtGeoms$polygon, x = 0.5, y = 0.2,
#'                        update = FALSE)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # stretch a single feature
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_stretch(obj = gtGeoms$polygon, x = 0.5, fid = 2, update = FALSE)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # stretch feature separately
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_stretch(obj = gtGeoms$polygon,
#'                       x = c(0.2, 1),
#'                       y = c(1, 0.2),
#'                       update = FALSE)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertLogical
#' @importFrom geomio getFeatures getGroups getPoints getWindow getNames getType
#'   getCRS getProvenance
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

geo_stretch <- function(obj, x = NULL, y = NULL, fid = NULL, update = TRUE){

  assertNumeric(x, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(y, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  verts <- getPoints(x = obj)

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
    hist <- paste0("geom was stretched.")
  } else {
    hist <- paste0("geoms were stretched.")
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
