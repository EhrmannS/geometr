#' Stretch geometries
#'
#' Stretch \code{geom}s by a scale factor in x and y-dimension.
#' @param geom [\code{geom}]\cr object of class \code{\link{geom}}.
#' @param x [\code{numeric(1)}]\cr the scale factor in x-dimension.
#' @param y [\code{numeric(1)}]\cr the scale factor in y-dimension.
#' @param fid [\code{integerish(.)}]\cr the features to stretch.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after stretching.
#' @return Stretched \code{geom}.
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
#' # stretch several geoms
#' visualise(geom = gt_stretch(geom = aGeom, x = list(0.5), y = list(1, 0.2)))
#'
#' # stretch single geom
#' visualise(geom = gt_stretch(geom = aGeom, x = 0.5, fid = 1))
#' @importFrom checkmate assertClass testList testNumeric assert
#' @importFrom methods new
#' @export

gt_stretch <- function(geom, x = NULL, y = NULL, fid = NULL, update = TRUE){

  assertClass(geom, classes = "geom")
  xIsList <- testList(x, types = "numeric", any.missing = FALSE)
  xIsNumeric <- testNumeric(x, any.missing = FALSE, len = 1, null.ok = TRUE)
  yIsList <- testList(y, types = "numeric", any.missing = FALSE)
  yIsNumeric <- testNumeric(y, any.missing = FALSE, len = 1, null.ok = TRUE)
  assert(xIsList, xIsNumeric)
  assert(yIsList, yIsNumeric)
  assertIntegerish(x = fid, any.missing = FALSE, null.ok = TRUE)

  if(length(geom) == 1){
    newHistory <- paste0("geometry was stretched")
  } else {
    newHistory <- paste0("geometries were stretched")
  }

  if(is.null(x)){
    x <- 1
  }
  if(is.null(y)){
    y <- 1
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
    doStretch <- ids %in% fid
  } else{
    doStretch <- rep(TRUE, length(ids))
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

    if(doStretch[i]){
      newCoords$x <- x[[i]] * tempCoords$x
      newCoords$y <- y[[i]] * tempCoords$y

      newCentroid <- tibble(x = mean(x = newCoords$x), y = mean(x = newCoords$y))
      offset <- newCentroid - centroid

      newCoords$x <- newCoords$x - offset$x
      newCoords$y <- newCoords$y - offset$y

      if(geom@type != "point"){
        newCoords <- bind_rows(newCoords, newCoords[1,])
        newCoords$vid <- seq_along(newCoords$fid)
      }
    }
    temp <- bind_rows(temp, newCoords)
  }

  if(update){
    inWindow <- pointInGeomC(vert = as.matrix(temp[c("x", "y")]),
                             geom = as.matrix(bind_rows(geom@window[c("x", "y")], geom@window[c("x", "y")][1,])),
                             invert = FALSE)
    if(!all(inWindow == 1)){
      window <- tibble(x = c(min(temp$x), max(temp$x)),
                       y = c(min(temp$y), max(temp$y)))
    } else {
      window <- geom@window
    }
  } else {
    window <- geom@window
  }

  out <- new(Class = "geom",
             type = geom@type,
             vert = temp,
             attr = geom@attr,
             window = window,
             scale = geom@scale,
             crs = geom@crs,
             history = c(geom@history, list(newHistory)))

  return(out)
}
