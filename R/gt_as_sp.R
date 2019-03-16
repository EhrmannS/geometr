#' Transform geometry to spatial object
#'
#' An object of class \code{Spatial*} is the sp-package representation of a
#' \code{geom}.
#' @param geom [\code{geom}]\cr Object of class \code{\link{geom}}.
#' @return Depending on the provided geometry either a
#'   \code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialLinesDataFrame}}
#'   or \code{\link{SpatialPolygonsDataFrame}} object.
#' @examples
#' (spPoints <- gt_as_sp(geom = gtGeoms$point))
#' (spLines <- gt_as_sp(geom = gtGeoms$line))
#' (spPolygon <- gt_as_sp(geom = gtGeoms$polygon))
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom raster crs
#' @importFrom sp SpatialPoints SpatialPointsDataFrame Line Lines SpatialLines
#'   SpatialLinesDataFrame Polygon Polygons SpatialPolygons
#'   SpatialPolygonsDataFrame
#' @export

gt_as_sp <- function(geom = NULL){

  assertClass(geom, classes = "geom")

  featureType <- geom@type
  vert <- geom@vert
  features <- unique(vert$fid)

  if(featureType %in% c("point")){

    temp <- vert[c("x", "y")]
    geomSp <- SpatialPoints(temp)
    # if there are columns in the geoms attribute table, make a Spatial*DataFrame
    if(!all(names(geom@attr) %in% c("fid", "gid"))){
      attr <- geom@attr[,!names(geom@attr) %in% c("fid", "gid")]
      geomSp <- SpatialPointsDataFrame(geomSp, data = attr, match.ID = FALSE)
    }

  } else if(featureType %in% c("line")){

    outLines <- list()
    outLine <- list()
    for(i in seq_along(features)){
      theLine <- vert[c("x", "y")][vert$fid == features[i],]
      outLine <- c(outLine, Line(theLine))
      outLines <- c(outLines, Lines(outLine, features[i]))
    }
    geomSp <- SpatialLines(outLines)

    # if there are columns in the geoms attribute table, make a Spatial*DataFrame
    if(!all(names(geom@attr) %in% c("fid", "gid"))){
      attr <- geom@attr[,!names(geom@attr) %in% c("fid", "gid")]
      geomSp <- SpatialLinesDataFrame(geomSp, data = attr, match.ID = FALSE)
    }

  } else if(featureType %in% c("polygon")){

    outPolygons <- list()
    # go through distinct ids and check whether the last coordinate is equal to the first.
    for(i in seq_along(features)){
      thePoly <- vert[c("x", "y")][vert$fid == features[i],]
      outPolygon <- list()

      dups <- as.numeric(duplicated(thePoly[c("x", "y")]))
      dups <- c(0, dups[-length(dups)])
      thePoly$subID <- 1 + cumsum(dups)

      for(j in seq_along(unique(thePoly$subID))){
        temp <- thePoly[c("x", "y")][thePoly$subID == j,]

        outPolygon <- c(outPolygon, Polygon(temp))
      }

      # put togehter the 'Polygons' list
      outPolygons <- c(outPolygons, Polygons(outPolygon, features[i]))
    }

    # make a SpatialPolygon out of that
    geomSp <- SpatialPolygons(outPolygons)
    if(!all(names(geom@attr) %in% c("fid", "gid"))){
      attr <- geom@attr[,!names(geom@attr) %in% c("fid", "gid")]
      geomSp <- SpatialPolygonsDataFrame(geomSp, data = attr, match.ID = FALSE)
    }

  }

  return(geomSp)
}