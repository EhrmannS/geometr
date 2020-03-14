#' Transform a spatial object to class \code{geom}
#'
#' @param input the object to transform to class \code{geom}.
#' @param group [\code{logical(1)}]\cr should the attributes of multi* features
#'   be grouped, i.e. should the unique values per multi* feature be assigned
#'   into the groups table (\code{TRUE}), or should they be kept as duplicated
#'   per-feature attributes (\code{FALSE}, default)?
#' @param ... additional arguments.
#' @return an object of class \code{geom}
#' @family spatial classes
#' @examples
#' gc_geom(input = gtPPP)
#'
#' gc_geom(input = gtSF$polygon)
#'
#' gc_geom(input = gtRasters$categorical)
#' @name gc_geom
#' @rdname gc_geom
NULL

# generic ----
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

# Spatial ----
#' @rdname gc_geom
#' @importFrom tibble tibble
#' @export
setMethod(f = "gc_geom",
          signature = "Spatial",
          definition = function(input = NULL, ...){

            theCoords <- getPoints(x = input)
            theData <- getFeatures(x = input)
            theWindow <- getWindow(x = input)
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
            theGroups <- tibble(gid = unique(theData$gid))

            history <- paste0("geom was transformed from an object of class '", sourceClass, "'.")

            out <- new(Class = "geom",
                       type = type,
                       point = theCoords,
                       feature = list(geometry = theData),
                       group = list(geometry = theGroups),
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)

# sf ----
#' @rdname gc_geom
#' @importFrom tibble tibble
#' @importFrom sf st_geometry_type
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "gc_geom",
          signature = "sf",
          definition = function(input = NULL, group = FALSE, ...){

            theCoords <- getPoints(x = input)
            theData <- getFeatures(x = input)
            theCRS <- getCRS(x = input)
            theWindow <- getWindow(x = input)

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
            history <- paste0("geom was transformed from an sf-object of geometry type '", sourceClass, "'.")

            out <- new(Class = "geom",
                       type = type,
                       point = theCoords,
                       feature = list(geometry = theData),
                       group = list(geometry = theGroups),
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)

# ppp ----
#' @rdname gc_geom
#' @importFrom tibble tibble
#' @export
setMethod(f = "gc_geom",
          signature = "ppp",
          definition = function(input = NULL, ...){

            theCoords <- getPoints(x = input)
            theData <- getFeatures(x = input)
            theGroups <- tibble(gid = theData$gid)
            theWindow <- getWindow(x = input)
            history <- paste0("geom was transformed from an object of class ppp.")
            theCRS <- NA_character_

            out <- new(Class = "geom",
                       type = "point",
                       point = theCoords,
                       feature = list(geometry = theData),
                       group = list(geometry = theGroups),
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)

# Raster ----
#' @rdname gc_geom
#' @importFrom tibble tibble
#' @importFrom raster xres yres
#' @importFrom utils object.size
#' @export
setMethod(f = "gc_geom",
          signature = "Raster",
          definition = function(input = NULL, ...){

            theExtent <- getExtent(x = input)
            theCoords <- tibble(x = c(min(theExtent$x), input@ncols, xres(input)),
                                y = c(min(theExtent$y), input@nrows, yres(input)))

            theType <- getType(x = input)
            theWindow <- getWindow(x = input)

            theFeatures <- list()
            theGroups <- hist <- list()
            for(i in 1:dim(input)[3]){
              hist <- c(hist, paste0("geom was transformed from an object of class ", theType[2], "."))

              theInput <- input[[i]]
              theName <- names(input)[i]

              rawVal <- getFeatures(x = theInput)$values
              rleVal <- rle(rawVal)
              if(object.size(rleVal) > object.size(rawVal)){
                tempFeatures <- tibble(values = rawVal)
              } else {
                tempFeatures <- tibble(val = rleVal$values,
                                       len = rleVal$lengths)
                hist <- c(hist, paste0("layer '", theName, "' is run-length encoded."))
              }
              theFeatures <- c(theFeatures, setNames(list(tempFeatures), theName))

              if(length(theInput@data@attributes) != 0){
                tempGroups <- as_tibble(theInput@data@attributes[[1]])
                colnames(tempGroups) <- c("gid", colnames(tempGroups)[-1])
              } else {
                tempGroups <- tibble(gid = integer())
              }
              theGroups <- c(theGroups, setNames(list(tempGroups), theName))

            }

            theCRS <- getCRS(x = input)

            out <- new(Class = "geom",
                       type = "grid",
                       point = theCoords,
                       feature = theFeatures,
                       group = theGroups,
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = c(getHistory(input), list(hist)))

            return(out)
          }
)
