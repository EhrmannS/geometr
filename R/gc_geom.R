#' Make a geometry object of class \code{geom}
#' @param input the object from which to make an object of class \code{geom}.
#' @param window [\code{data.frame(1)}]\cr the reference window of the new geom.
#' @param ... additional arguments, such as \code{verbose = TRUE/FALSE}.
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
             def = function(input, window = NULL, ...){
               standardGeneric("gc_geom")
             }
  )
}

#' @param group [\code{logical(1)}]\cr should the attributes of multi* features
#'   be grouped, i.e. should the unique values per multi* feature be assigned
#'   into the groups table (\code{TRUE}), or should they be kept as
#'   duplicated per-feature attributes (\code{FALSE}, default)?
#' @rdname gc_geom
#' @importFrom tibble tibble as_tibble
#' @importFrom sf st_geometry_type
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "gc_geom",
          signature = "sf",
          definition = function(input, window = NULL, group = FALSE, ...){

            window <- .testWindow(x = window, ...)

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
            if(sourceClass %in% c("MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON") & group){
              temp <- theData[-which(colnames(theData) %in% c("fid", "gid"))]
              temp <- temp[!duplicated(temp),]
              theGroups <- bind_cols(gid = 1:dim(temp)[1], temp)
              theData <- theData[c("fid", "gid")]
            } else {
              theGroups <- tibble(gid = unique(theData$gid))
            }
            history <- paste0("geometry was transformed from an sf-object of geometry type '", sourceClass, "'.")
            theCRS <- getCRS(x = input)

            out <- new(Class = "geom",
                       type = type,
                       vert = theCoords,
                       feat = theData,
                       group = theGroups,
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
          definition = function(input, window = NULL, ...){

            window <- .testWindow(x = window, ...)

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
            history <- paste0("geometry was transformed from an object of class '", sourceClass, "'.")

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
