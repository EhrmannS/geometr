#' Transform a spatial object to class \code{geom}
#'
#' @param input the object to transform to class \code{geom}.
#' @param group [\code{logical(1)}]\cr should the values of a Raster* or the
#'   attributes of MULTI* features be grouped, i.e., should the unique values be
#'   assigned into the groups table (\code{TRUE})? The default behaviour for
#'   Raster* would be not to assign values into the group attribute table if no
#'   RAT is available and for MULIT* features it would be to keep the attributes
#'   as duplicated per-feature attributes (\code{FALSE})?
#' @param stack [\code{logical(1)}]\cr should the layers of gridded objects be
#'   stacked, i.e., should several layers be stored as columns in the attribute
#'   table of features of one geom (\code{TRUE}, default), or should they be
#'   stored in (a list of) several geoms separately (\code{FALSE})?
#' @param as_hex [\code{logical(1)}]\cr should the bands 'red', 'green' and
#'   'blue' of a gridded object be transformed to hexadecimal values
#'   (\code{TRUE}), or should they be retained as columns in a stacked grid geom
#'   (\code{FALSE}, default)?.
#' @param ... additional arguments.
#' @details When transforming a simple feature to a geom, all MULTI* features
#'   are organised on a per feature basis, where the attribute table of features
#'   in the geom contains those variables that are valid for each feature, while
#'   the attribute table of groups contains those variables, that are unique
#'   only at the level of groups of features (i.e., at the level of MULTI*
#'   simple features). Those variables that are valid at the level of groups
#'   would be duplicated in the attribute table of features. When a MULTI*
#'   feature is transformed to a geom, the default behaviour is to copy the
#'   simple feature as closely as possible. However, to reduce the object size
#'   (and improve its' organisation), it is possible to assign the attributes of
#'   groups into the attribute table of groups of the geom by setting
#'   \code{group = TRUE}.
#'
#'   When transforming a Raster* (or possibly other gridded classes) with
#'   several layers to a geom, the layers are by default organised into a list
#'   with a layer per list item. However, when several layers contain
#'   fundamentally the same data (i.e., values that are associated to the same
#'   groups), layers could be stacked \code{stack = TRUE}, because they share
#'   the same group attribute table.
#' @return an object of class \code{geom}
#' @family spatial classes
#' @examples
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
                       feature = theData,
                       group = theGroups,
                       window = theWindow,
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
                       feature = theData,
                       group = theGroups,
                       window = theWindow,
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)

# Raster ----
#' @rdname gc_geom
#' @importFrom tibble tibble
#' @importFrom raster xres yres getValues
#' @importFrom dplyr bind_cols full_join arrange
#' @importFrom utils object.size
#' @export
setMethod(f = "gc_geom",
          signature = "Raster",
          definition = function(input = NULL, stack = FALSE, group = FALSE,
                                as_hex = FALSE, ...){

            theExtent <- getExtent(x = input)
            theCoords <- tibble(x = c(min(theExtent$x), input@ncols, xres(input)),
                                y = c(min(theExtent$y), input@nrows, yres(input)))

            theType <- getType(x = input)
            theWindow <- getWindow(x = input)
            theCRS <- getCRS(x = input)

            if(dim(input)[3] == 1){
              stack <- FALSE
            }

            assertLogical(x = as_hex, len = 1)
            if(as_hex){
              assertNames(x = names(input), must.include = c("red", "green", "blue"))
              temp <- getLayers(x = input)
              red <- getFeatures(x = temp[["red"]])$values
              red[is.na(red)] <- 255L
              green <- getFeatures(x = temp[["green"]])$values
              green[is.na(green)] <- 255L
              blue <- getFeatures(x = temp[["blue"]])$values
              blue[is.na(blue)] <- 255L
              alpha <- rep(255, length(blue))
              alpha[is.na(red)] <- 0L
              alpha[is.na(green)] <- 0L
              alpha[is.na(blue)] <- 0L

              input <- input[[1]] # subset to have dim(input) == 1
              names(input) <- "colours"
            }

            out <- theFeatures <- NULL
            theGroups <- tibble(gid = integer())
            for(i in 1:dim(input)[3]){

              theInput <- input[[i]]
              theName <- names(input)[i]
              hist <- paste0("layer '", theName, "' was transformed from an object of class ", theType[2], ".")

              if(as_hex){
                rawVal <- rgb(red = red, green = green, blue = blue, alpha = alpha, maxColorValue = 255)
              } else {
                rawVal <- getFeatures(x = theInput)$values
              }
              tempGroups <- getGroups(theInput)
              if(group & dim(tempGroups)[1] == 0) {
                tempGroups <- tibble(gid = sortUniqueC(getValues(theInput)))
              }

              if(stack){

                tempFeatures <- tibble(rawVal)
                names(tempFeatures) <- theName
                theFeatures <- bind_cols(theFeatures, tempFeatures)
                theGroups <- full_join(theGroups, tempGroups, by = "gid")
                theGroups <- arrange(theGroups, gid)

              } else {

                rleVal <- rle(rawVal)
                if(object.size(rleVal) > object.size(rawVal)){
                  theFeatures <- tibble(rawVal)
                  names(theFeatures) <- theName
                } else {
                  theFeatures <- tibble(val = rleVal$values,
                                        len = rleVal$lengths)
                  hist <- c(hist, paste0("layer '", theName, "' is run-length encoded."))
                }
                theGroups <- tempGroups

                temp <- new(Class = "geom",
                            type = "grid",
                            point = theCoords,
                            feature = theFeatures,
                            group = theGroups,
                            window = theWindow,
                            crs = theCRS,
                            history = c(getHistory(input), hist))

                if(dim(input)[3] != 1){
                  out <- c(out, setNames(list(temp), theName))
                } else {
                  out <- temp
                }
              }
            }

            if(stack){

              out <- new(Class = "geom",
                          type = "grid",
                          point = theCoords,
                          feature = theFeatures,
                          group = theGroups,
                          window = theWindow,
                          crs = theCRS,
                          history = c(getHistory(input), hist))
            }

            return(out)
          }
)
