#' Derive a \code{geom} from other spatial objects
#'
#' See \code{\link{geom}} for details on the class.
#' @param input [\code{Spatial*} | \code{sf}]\cr the spatial object to build a
#'   \code{geom} from.
#' @details bla
#' @return a \code{geom} of the type that comes closest to the type of the
#'   input.
#' @examples
#' library(raster)
#'
#' spPoly <- gtSP$SpatialPolygons
#' plot(spPoly)
#' visualise(geom = gt_as_geom(input = spPoly))
#'
#' sfPoly <- gtSF$polygon
#' plot(sfPoly)
#' visualise(geom = gt_as_geom(input = sfPoly))
#'
#' @importFrom checkmate assertClass testDataFrame
#' @importFrom methods as
#' @importFrom tibble tibble
#' @importFrom sf st_geometry_type
#' @export

gt_as_geom <- function(input){

  # check arguments
  isSp <- testClass(input, classes = "Spatial")
  isSf <- testClass(input, classes = "sf")

  bbox <- getExtent(x = input)
  theCoords <- getVertices(x = input)
  theData <- getTable(x = input)
  theWindow <- tibble(x = rep(c(bbox$x), each = 2),
                      y = c(bbox$y, rev(bbox$y)))
  theCRS <- getCRS(x = input)

  if(isSp){

    sourceClass <- class(input)[1]
    if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialPixels", "SpatialPixelsDataFrame")){
      type <- "point"
    } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
      type <- "point"
    } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
      type <- "line"
    } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialGrid", "SpatialGridDataFrame")){
      type <- "polygon"
    }
    history <- paste0("geometry was created from an object of class '", sourceClass, "'")

  } else if(isSf){

    sourceClass <- st_geometry_type(input)
    if(length(unique(sourceClass)) == 1){
      sourceClass <- unique(sourceClass)
    } else{
      # what happens if a sf-object has different feature-types?
    }
    if(sourceClass %in% c("POINT", "MULTIPOINT")){
      type <- "point"
    } else if(sourceClass %in% c("LINESTRING", "MULTILINESTRING")){
      type <- "line"
    } else if(sourceClass %in% c("POLYGON", "MULTIPOLYGON")){
      type <- "polygon"
    }
    history <- paste0("geometry was created from an sf-object of geometry type '", sourceClass, "'")
  }

  theGeom <- new(Class = "geom",
                 type = type,
                 vert = theCoords,
                 attr = theData,
                 window = theWindow,
                 scale = "absolute",
                 crs = theCRS,
                 history = list(history))

  return(theGeom)

}