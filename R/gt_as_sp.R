#' Transform geometry to spatial object
#'
#' An object of class \code{Spatial*} is the sp-package representation of a
#' \code{geom}.
#' @param geom [\code{geom}]\cr Object of class \code{\link{geom}}.
#' @param crs [\code{character(1)}]\cr corrdinate reference system of the
#'   object in proj4 notation.
#' @return Depending on the provided geometry either a
#'   \code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialLinesDataFrame}}
#'   or \code{\link{SpatialPolygonsDataFrame}} object.
#' @examples
#' library(magrittr)
#'
#' somePoints <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
#'                          5234735, 5281527, 5189955, 5041066),
#'                          Y = c(3977612, 3971119, 4028167, 3997230,
#'                          4060164, 4117856, 4118207, 4062838),
#'                          fid = c(1:8))
#'
#' pointsGeom <- gs_point(anchor = somePoints)
#' polyGeom <- gt_group(geom = pointsGeom, index = c(rep(1, 8))) %>%
#'   gs_polygon()
#'
#' (spPoints <- gt_as_sp(geom = pointsGeom,
#'   crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
#' (spPolygon <- gt_as_sp(geom = polyGeom,
#'   crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom raster crs
#' @importFrom sp SpatialPoints SpatialPointsDataFrame Polygon Polygons
#'   SpatialPolygons SpatialPolygonsDataFrame
#' @export

gt_as_sp <- function(geom, crs = NULL){

  assertClass(geom, classes = "geom")
  assertCharacter(crs, fixed = "+proj", null.ok = TRUE)
  if(is.null(crs) & is.na(geom@crs)){
    targetCRS <- as.character(NA)
  } else if(!is.null(crs)){
    targetCRS <- crs
  } else if(!is.na(geom@crs)){
    targetCRS <- geom@crs
  }
  if(is.na(geom@crs)){
    sourceCrs <- as.character(NA)
  } else{
    sourceCrs <- geom@crs
  }

  featureType <- geom@type
  coords <- geom@coords
  features <- unique(coords$fid)

  if(featureType %in% c("point")){

    temp <- coords[c("x", "y")]
    geomSp <- SpatialPoints(temp, proj4string = crs(sourceCrs))
    if(!all(names(geom@attr) %in% c("fid", "n"))){
      attr <- geom@attr[,!names(geom@attr) %in% c("fid", "n")]
      geomSp <- SpatialPointsDataFrame(geomSp, data = attr, match.ID = FALSE)
    }

    # } else if(featureType %in% c("line")){
    #
    #   temp <- list()
    #   # go through distinct ids and check whether the last coordinate is equal to the first.
    #   for(i in seq_along(id)){
    #     thePoly <- coords[c(1, 2)][coords$id == id[i],]
    #     if(!all(thePoly[1,] == thePoly[dim(thePoly)[1],])){
    #       thePoly <- rbind(thePoly, thePoly[1,])
    #     }
    #
    #     # put togehter the 'Polygons' list
    #     temp <- c(temp, Polygons(list(Polygon(thePoly)), id[i]))
    #   }
    #
    #   geomSp <- SpatialLines(temp, proj4string = CRS(theCRS))
    #   geomSp <- SpatialLinesDataFrame(geomSp, data = data.frame(id = seq_along(geomSp)), match.ID = FALSE)

  } else if(featureType %in% c("polygon")){

    temp <- list()
    # go through distinct ids and check whether the last coordinate is equal to the first.
    for(i in seq_along(features)){
      thePoly <- coords[c("x", "y")][coords$fid == features[i],]
      if(!all(thePoly[1,] == thePoly[dim(thePoly)[1],])){
        thePoly <- rbind(thePoly, thePoly[1,])
      }

      # put togehter the 'Polygons' list
      temp <- c(temp, Polygons(list(Polygon(thePoly)), features[i]))
    }

    # make a SpatialPolygon out of that
    geomSp <- SpatialPolygons(temp, proj4string = crs(sourceCrs))
    if(!all(names(geom@attr) %in% c("fid", "n"))){
      attr <- geom@attr[,!names(geom@attr) %in% c("fid", "n")]
      geomSp <- SpatialPolygonsDataFrame(geomSp, data = attr, match.ID = FALSE)
    }

  }
  geomSp <- setCRS(x = geomSp, crs = targetCRS)

  return(geomSp)
}