#' Make a geometry object of class \code{geom}
#' @param input the object from which to make an object of class \code{geom}.
#' @return an object of class \code{geom}
#' @family spatial classes
#' @examples
#' geomPoly <- gc_geom(input = gtSF$polygon)
#' geomLine <- gc_geom(input = gtSP$SpatialLinesDataFrame)
#' @name gc_geom
#' @rdname gc_geom
NULL

#' @rdname gc_geom
#' @name gc_geom
#' @export
if(!isGeneric("gc_geom")){
  setGeneric(name = "gc_geom",
             def = function(input, ...){
               standardGeneric("gc_geom")
             }
  )
}

#' @rdname gc_geom
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "gc_geom",
          signature = "sf",
          definition = function(input, ...){

            theCoords <- getVertices(x = input)
            theData <- getTable(x = input)
            theCRS <- getCRS(x = input)
            bbox <- getExtent(x = input)
            theWindow = tibble(x = c(min(bbox$x), max(bbox$x), max(bbox$x), min(bbox$x), min(bbox$x)),
                               y = c(min(bbox$y), min(bbox$y), max(bbox$y), max(bbox$y), min(bbox$y)))

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
            theCRS <- getCRS(x = input)

            out <- new(Class = "geom",
                       type = type,
                       vert = theCoords,
                       feat = theData,
                       group = tibble(gid = theData$gid),
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)

#' @rdname gc_geom
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "gc_geom",
          signature = "Spatial",
          definition = function(input, ...){

            theCoords <- getVertices(x = input)
            theData <- getTable(x = input)
            theCRS <- getCRS(x = input)
            bbox <- getExtent(x = input)
            theWindow = tibble(x = c(min(bbox$x), max(bbox$x), max(bbox$x), min(bbox$x), min(bbox$x)),
                               y = c(min(bbox$y), min(bbox$y), max(bbox$y), max(bbox$y), min(bbox$y)))
            theCRS <- getCRS(x = input)

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

            out <- new(Class = "geom",
                       type = type,
                       vert = theCoords,
                       feat = theData,
                       group = tibble(gid = theData$gid),
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)

#' @rdname gc_geom
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "gc_geom",
          signature = "grob",
          definition = function(input, ...){

            sourceClass <- class(input)[1]
            if(sourceClass == "pointsGrob"){
              type <- "point"
            } else if(sourceClass == "polylineGrob"){
              type <- "line"
            } else if(sourceClass == "pathgrob"){
              type <- "polygon"
            }

            theCoords <- tibble(fid = 1, vid = seq_along(input$x), x = as.numeric(input$x), y = as.numeric(input$y))
            if(is.null(window)){
              theWindow <- tibble(x = rep(c(min(theCoords$x), max(theCoords$x)), each = 2),
                                  y = c(min(theCoords$y), max(theCoords$y), max(theCoords$y), min(theCoords$y)))
              theScale <- "absolute"
            } else {
              theWindow <- tibble(x = rep(c(as.numeric(window$x), as.numeric(window$x) + as.numeric(window$width)), each = 2),
                                  y = c(as.numeric(window$y), as.numeric(window$y) + as.numeric(window$height), as.numeric(window$y) + as.numeric(window$height), as.numeric(window$y)))
              theScale <- "relative"
            }
            theData <- tibble(fid = 1, gid = 1)
            history <- paste0("geometry was created with gt_grob() from an object of class '", sourceClass, "'")

            out <- new(Class = "geom",
                       type = type,
                       vert = theCoords,
                       attr = theData,
                       window = theWindow,
                       scale = theScale,
                       crs = NA_character_,
                       history = list(history))
            if(theScale == "relative"){
              out <- gt_scale(geom = out, to = "absolute")
            }

            return(out)
          }
)
