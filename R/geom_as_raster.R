#' Transform geometry to raster
#'
#' An object of class \code{RasterLayer} is the raster-package representation of
#' a \code{geom}.
#' @template geom
#' @param negative [\code{logical(1)}]\cr should the area covered by \code{geom}
#'   be set to 0 (\code{TRUE}) or should it be set to 1 (\code{FALSE}, default)?
#' @param res [\code{numeric(2)}]\cr resolution in x and y direction.
#' @param crs [\code{character(1)}]\cr corrdinate reference system of the
#'   object in proj4 notation.
#' @return a binary \code{\link{raster}} with the dimensions of the reference
#'   window of \code{geom} and the resolution \code{res}.
#' @examples
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70),
#'                      fid = 1)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window)
#'
#' aRaster <- gToRaster(geom = aGeom)
#' visualise(raster = aRaster, geom = aGeom, col = "deeppink")
#'
#' negRaster <- gToRaster(geom = aGeom, negative = TRUE)
#' visualise(raster = negRaster, geom = aGeom, col = "deeppink")
#' @importFrom methods new
#' @importFrom raster raster extent<-
#' @export

gToRaster <- function(geom, negative = FALSE, res = c(1, 1), crs = NULL){

  assertClass(geom, classes = "geom")
  assertLogical(negative)
  assertNumeric(res, len = 2, finite = TRUE)
  assertCharacter(crs, fixed = "+proj", null.ok = TRUE)
  if(!is.null(crs)){
    targetCRS <- crs
  } else{
    targetCRS <- NA
  }
  if(!is.na(geom@crs)){
    sourceCRS <- geom@crs
  } else{
    sourceCRS <- NA
  }

  theWindow <- getWindow(geom)
  extCols <- round(c(min(theWindow$x, na.rm = TRUE), max(theWindow$x, na.rm = TRUE))/res[1])
  outCols <- round(max(extCols, na.rm = TRUE) - min(extCols, na.rm = TRUE))
  extRows <- round(c(min(theWindow$y, na.rm = TRUE), max(theWindow$y, na.rm = TRUE))/res[2])
  outRows <- round(max(extRows, na.rm = TRUE) - min(extRows, na.rm = TRUE))

  temp <- matrix(data = 0, ncol = outCols, nrow = outRows)
  coords <- geom@coords[c("x", "y")]
  coords[,1] <- round(coords[,1]/res[1])
  coords[,2] <- round(coords[,2]/res[2])
  vertices <- as.matrix(coords)
  if(!any(theWindow$x == 0)){
    vertices[,1] <- vertices[,1] - min(vertices[,1], na.rm = TRUE)
  }
  if(!any(theWindow$y == 0)){
    vertices[,2] <- vertices[,2] - min(vertices[,2], na.rm = TRUE)
  }
  if(any(coords[dim(coords)[1],] != coords[1,])){
    vertices <- rbind(vertices, vertices[1,])
  }
  geomRaster <- matInGeomC(mat = temp, geom = vertices, negative = negative)
  out <- raster(geomRaster, xmn = 0, xmx = outCols, ymn = 0, ymx = outRows, crs = as.character(sourceCRS))
  extent(out) <- extent(extCols[1]*res[1], extCols[2]* res[1], extRows[1]*res[2], extRows[2]*res[2])

  out@history <- c(geom@history, list(paste0("geometry was transformed to a raster")))

  if(!is.na(targetCRS)){
    out <- setCRS(x = out, crs = targetCRS)
  }

  return(out)
}
