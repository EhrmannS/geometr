#' Skew geometric objects
#'
#' Skew geometric objects by a shear factor in x and y dimension.
#' @param obj [gridded(1)][geom]\cr the object to skew.
#' @param x [numeric(1)][numeric]\cr the shear factor in x dimension.
#' @param y [numeric(1)][numeric]\cr the shear factor in y dimension.
#' @param fid [integerish(.)][integer]\cr in case only a subset of features shall
#'   be skewed, specify that here.
#' @param update [logical(1)][logical]\cr whether or not to update the window slot
#'   after skewing.
#' @return \code{geom} of the skewed \code{obj}.
#' @family geometry tools
#' @examples
#' # skew several features
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_skew(obj = gtGeoms$polygon, x = 0.5, update = FALSE)
#' geo_vis(geom = newPoly, linecol = "green", new = FALSE)
#'
#' # skew a single feature
#' geo_vis(gtGeoms$polygon, linewidth = 3)
#' newPoly <- geo_skew(obj = gtGeoms$polygon, x = 0.5, y = .7, fid = 2,
#'                    update = FALSE)
#' geo_vis(newPoly, linecol = "green", new = FALSE)
#' @importFrom checkmate assertNumeric assertLogical
#' @importFrom geomio getFeatures getGroups getPoints getWindow getNames getType
#'   getProvenance
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

geo_skew <- function(obj, x = NULL, y = NULL, fid = NULL, update = TRUE){

  assertNumeric(x = x, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  assertNumeric(x = y, any.missing = FALSE, min.len = 1, null.ok = TRUE)
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
    hist <- paste0("geom was skewed.")
  } else {
    hist <- paste0("geoms were skewed.")
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