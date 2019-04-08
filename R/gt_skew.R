#' Skew geometries
#'
#' Skew \code{geom}s by a shear factor in x and y-dimension.
#' @param geom [\code{geom}]\cr object of class \code{\link{geom}}.
#' @param x [\code{numeric(1)}]\cr the shear factor in x-dimension.
#' @param y [\code{numeric(1)}]\cr the shear factor in y-dimension.
#' @param fid [\code{integerish(.)}]\cr vector of features that should be
#'   skewed.
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
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom methods new
#' @export

gt_skew <- function(geom, x = NULL, y = NULL, fid = NULL){

  assertClass(geom, classes = "geom")
  xIsList <- testList(x, types = "numeric", any.missing = FALSE)
  xIsNumeric <- testNumeric(x, any.missing = FALSE, len = 1, null.ok = TRUE)
  yIsList <- testList(y, types = "numeric", any.missing = FALSE)
  yIsNumeric <- testNumeric(y, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert(xIsList, xIsNumeric)
  assert(yIsList, yIsNumeric)

  if(length(geom) == 1){
    newHistory <- paste0("geometry was skewed")
  } else {
    newHistory <- paste0("geometries were skewed")
  }

  if(is.null(x)){
    x <- 0
  }
  if(is.null(y)){
    y <- 0
  }

  if(xIsNumeric){
    x <- list(x)
  }
  if(yIsNumeric){
    y <- list(y)
  }

  verts <- geom@vert
  ids <- unique(verts$fid)

  existsID <- !is.null(fid)
  if(existsID){
    doSkew <- ids %in% fid
  } else{
    doSkew <- rep(TRUE, length(ids))
  }

  if(length(x) != length(ids)){
    x <- rep(x, length.out = length(ids))
  }
  if(length(y) != length(ids)){
    y <- rep(y, length.out = length(ids))
  }

  verts <- verts[!duplicated(verts[c("x", "y")]),]

  temp <- NULL
  for(i in seq_along(ids)){
    tempCoords <- verts[verts$fid == ids[i],]
    newCoords <- tempCoords

    centroid <- tibble(x = mean(x = tempCoords$x), y = mean(x = tempCoords$y))

    if(doSkew[i]){
      newCoords$x <- tempCoords$x + x[[i]] * tempCoords$y
      newCoords$y <- tempCoords$y + y[[i]] * tempCoords$x

      newCentroid <- tibble(x = mean(x = newCoords$x), y = mean(x = newCoords$y))
      offset <- newCentroid - centroid

      newCoords$x <- newCoords$x - offset$x
      newCoords$y <- newCoords$y - offset$y

      newCoords <- bind_rows(newCoords, newCoords[1,])
      newCoords$vid <- seq_along(newCoords$fid)
    }
    temp <- bind_rows(temp, newCoords)
  }

  out <- new(Class = "geom",
             type = geom@type,
             vert = temp,
             attr = geom@attr,
             window = geom@window,
             scale = geom@scale,
             crs = geom@crs,
             history = c(geom@history, list(newHistory)))

  return(out)
}