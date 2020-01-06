#' Skew \code{geom}s
#'
#' Skew \code{geom}s by a shear factor in x and y-dimension.
#' @param geom [\code{geom(.)}]\cr the object to skew.
#' @param x [\code{numeric(1)}]\cr the shear factor in x-dimension.
#' @param y [\code{numeric(1)}]\cr the shear factor in y-dimension.
#' @param fid [\code{integerish(.)}]\cr if only a subset of features shall be
#'   skewed, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after skewing.
#' @return Skewed \code{geom}.
#' @family geometry tools
#' @examples
#' # the original object
#' coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
#'                      y = c(40, 40, 60, 70, 10, 20, 40),
#'                      fid = c(1, 1, 1, 1, 2, 2, 2))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#'
#' # skew several geoms
#' visualise(geom = gt_skew(geom = aGeom, x = list(0.5), y = list(0, 0.2)))
#'
#' # skew single geom
#' visualise(geom = gt_skew(geom = aGeom, x = 0.5, fid = 1))
#' @importFrom checkmate assertClass assertNumber
#' @importFrom tibble tibble as_tibble
#' @importFrom methods new
#' @export

gt_skew <- function(geom, x = NULL, y = NULL, fid = NULL, update = TRUE){

  assertClass(geom, classes = "geom")
  xIsList <- testList(x, types = "numeric", any.missing = FALSE)
  xIsNumeric <- testNumeric(x, any.missing = FALSE, len = 1, null.ok = TRUE)
  yIsList <- testList(y, types = "numeric", any.missing = FALSE)
  yIsNumeric <- testNumeric(y, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert(xIsList, xIsNumeric)
  assert(yIsList, yIsNumeric)
  assertIntegerish(x = fid, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)

  theFeatures <- getFeatures(x = geom)
  theGroups <- getGroups(x = geom)

  # set default values
  if(is.null(x)){
    x <- 0
  }
  if(is.null(y)){
    y <- 0
  }

  # make list, if it is not yet
  if(xIsNumeric){
    x <- list(x)
  }
  if(yIsNumeric){
    y <- list(y)
  }

  verts <- getPoints(x = geom)
  thewindow <- getWindow(x = geom)
  ids <- unique(verts$fid)

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
    tempCoords <- verts[verts$fid == ids[i],]
    newCoords <- tempCoords

    if(doSkew[i]){
      centroid <- tibble(x = mean(x = tempCoords$x), y = mean(x = tempCoords$y))

      newCoords$x <- tempCoords$x + x[[i]] * tempCoords$y
      newCoords$y <- tempCoords$y + y[[i]] * tempCoords$x

      newCentroid <- tibble(x = mean(x = newCoords$x), y = mean(x = newCoords$y))
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
             type = getType(x = geom)[2],
             point = as_tibble(temp),
             feature = list(geometry = theFeatures),
             group = list(geometry = theGroups),
             window = window,
             scale = geom@scale,
             crs = getCRS(x = geom),
             history = c(getHistory(x = geom), list(hist)))

  return(out)
}