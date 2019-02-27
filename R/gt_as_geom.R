#' Derive a \code{geom} from other spatial objects
#'
#' See \code{\link{geom-class}} for details on differences between objects of
#' class \code{geom} and other spatial classes.
#' @param input [\code{Spatial*} | \code{sf}]\cr the spatial object to build a
#'   \code{geom} from.
#' @details bla
#' @return a \code{geom} of the type that comes closest to the type of the
#'   input.
#' @examples
#' library(raster)
#'
#' spPoly <- gtSP$SpatialPolygons
#' plot(spPoly, col = "goldenrod")
#' visualise(geom = gt_as_geom(input = spPoly))
#'
#' sfPoly <- gtSF$polygon
#' plot(sfPoly, col = "goldenrod")
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
  theCoords <- getCoords(x = input)
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
                 coords = theCoords,
                 attr = theData,
                 window = theWindow,
                 scale = "absolute",
                 crs = theCRS,
                 history = list(history))

  return(theGeom)

}

# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialPoints",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "point",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialPointsDataFrame",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "point",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialPixels",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "point",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialPixelsDataFrame",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "point",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialMultiPoints",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "point",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialMultiPointsDataFrame",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "point",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialLines",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "line",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialLinesDataFrame",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "line",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialPolygons",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "polygon",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialPolygonsDataFrame",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "polygon",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialGrid",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "polygon",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
#
# #' @rdname gt_as_geom
# #' @importFrom methods new
# #' @importFrom tibble tibble
# #' @export
# setMethod(f = "gt_as_geom",
#           signature = "SpatialGridDataFrame",
#           definition = function(input){
#             sourceClass <- class(input)[1]
#             bbox <- getExtent(x = input)
#             theCoords <- getCoords(x = input)
#             theData <- getTable(x = input)
#             theWindow <- tibble(x = rep(c(bbox$x), each = 2),
#                                 y = c(bbox$y, rev(bbox$y)))
#             theCRS <- getCRS(x = input)
#             theHistory <- paste0("geometry was created from an object of class '", sourceClass, "'")
#
#             theGeom <- new(Class = "geom",
#                            type = "polygon",
#                            coords = theCoords,
#                            attr = theData,
#                            window = theWindow,
#                            scale = "absolute",
#                            crs = theCRS,
#                            history = list(theHistory))
#           }
# )
