#' Get the attribute table of a spatial object.
#'
#' @param x the object from which to derive the attribute table.
#' @return A table of the attributes of \code{x}.
#' @name getTable
#' @rdname getTable
NULL

# generic ----
#' @rdname getTable
#' @name getTable
#' @export
if(!isGeneric("getTable")){
  setGeneric(name = "getTable",
             def = function(x, ...){
               standardGeneric("getTable")
             }
  )
}

# geom ----
#' @rdname getTable
#' @param slot [\code{character(1)}]\cr the geom slot from which to get the
#'   attribute table, either \code{"vert"}, \code{"feat"} or \code{"group"}. If
#'   left at \code{NULL}, 'feat' and 'group' will be joined.
#' @examples
#' getTable(gtGeoms$polygon)
#' @importFrom tibble as_tibble
#' @importFrom dplyr left_join
#' @export
setMethod(f = "getTable",
          signature = "geom",
          definition = function(x, slot = NULL){
            assertChoice(x = slot, choices = c("vert", "feat", "group"), null.ok = TRUE)
            if(is.null(slot)){
              left_join(x = x@feat, y = x@group, by = "gid")
            } else {
              if(slot == "vert"){
                as_tibble(x@vert)
              } else if(slot == "feat"){
                as_tibble(x@feat)
              } else {
                as_tibble(x@group)
              }
            }
          }
)

# Spatial ----
#' @rdname getTable
#' @examples
#'
#' getTable(x = gtSP$SpatialPolygons)
#' @importFrom methods as
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getTable",
          signature = signature("Spatial"),
          definition = function(x){

            theData <- NULL
            sourceClass <- class(x)[1]
            if(sourceClass %in% c("SpatialGrid")){
              sourceClass <- "SpatialPolygons"
            } else if(sourceClass %in% "SpatialGridDataFrame"){
              sourceClass <- "SpatialPolygonsDataFrame"
            } else if(sourceClass %in% "SpatialPixels"){
              sourceClass <- "SpatialPoints"
            } else if(sourceClass %in% "SpatialPixelsDataFrame"){
              sourceClass <- "SpatialPointsDataFrame"
            }
            x <- as(x, sourceClass)
            prev <- 0

            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame")){
              type <- "point"

              if(sourceClass %in% "SpatialPointsDataFrame"){
                theData <- tibble(fid = seq_along(x@coords[,1]),
                                  gid = seq_along(x@coords[,1]))
                theData <- bind_cols(theData, x@data)
              } else{
                theData <- tibble(fid = seq_along(x@coords[,1]),
                                  gid = seq_along(x@coords[,1]))
              }

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
              type <- "point"

              for(i in seq_along(x@coords)){
                tempCoords <- x@coords[[i]]

                if(sourceClass %in% "SpatialMultiPointsDataFrame"){
                  tempData <- tibble(fid = seq_along(tempCoords[,1])+prev,
                                     gid = i,
                                     x@data[i,])
                  j <- length(tempCoords[,1])

                  theData <- bind_rows(theData, tempData)
                  otherNames <- colnames(x@data)
                } else{
                  tempData <- tibble(fid = seq_along(tempCoords[,1])+prev,
                                     gid = i)
                  j <- length(tempCoords[,1])
                  theData <- bind_rows(theData, tempData)
                  otherNames <- NULL
                }
              }
              colnames(theData) <- c("fid", "gid", otherNames)

            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
              type <- "line"

              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]

                for(j in seq_along(theLines@Lines)){
                  if(sourceClass %in% "SpatialLinesDataFrame"){
                    tempData <- tibble(fid = prev + j, gid = prev + j, x@data[i,])
                    theData <- bind_rows(theData, tempData)
                    otherNames <- colnames(x@data)
                  } else{
                    theData <- bind_rows(theData, tibble(fid = prev + j, gid = prev + j))
                    otherNames <- NULL
                  }
                }
                prev <- prev + length(theLines@Lines)

              }
              colnames(theData) <- c("fid", "gid", otherNames)

            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
              type <- "polygon"

              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]

                if(sourceClass %in% "SpatialPolygonsDataFrame"){
                  tempData <- tibble(x@data[i,])
                  otherNames <- colnames(x@data)
                } else{
                  tempData <- NULL
                  otherNames <- NULL
                }

                for(j in seq_along(thePolys@Polygons)){
                  polyCoords <- thePolys@Polygons[[j]]@coords
                  polyCoords <- polyCoords[!duplicated(polyCoords),]
                }
                tempData <- bind_cols(tibble(fid = i, gid = i), tempData)

                theData <- bind_rows(theData, tempData)

              }
              colnames(theData) <- c("fid", "gid", otherNames)

            }
            return(theData)
          })

# sf ----
#' @rdname getTable
#' @examples
#'
#' getTable(gtSF$multiline)
#' @importFrom tibble tibble as_tibble
#' @importFrom sf st_geometry_type st_coordinates st_geometry<-
#' @export
setMethod(f = "getTable",
          signature = "sf",
          definition = function(x){

            sourceClass <- st_geometry_type(x)
            theCoords <- st_coordinates(x)
            if(length(unique(sourceClass)) == 1){
              sourceClass <- unique(sourceClass)
              if(sourceClass %in% c("POINT")){

                data <- x
                st_geometry(data) <- NULL
                fids <- seq_along(theCoords[, 1])
                new <- tibble(fid = fids, gid = fids)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "gid", names(data))

              } else if(sourceClass %in% c("MULTIPOINT")){

                data <- x
                st_geometry(data) <- NULL
                fids <- seq_along(theCoords[, 1])
                gids <- theCoords[, 3]
                out <- tibble(fid = fids, gid = gids, theCoords[,3])
                colnames(out) <- c("fid", "gid", names(data))

              } else if(sourceClass %in% c("LINESTRING")){

                data <- x
                st_geometry(data) <- NULL
                fids <- unique(theCoords[, 3])
                new <- tibble(fid = fids, gid = fids)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "gid", names(data))

              } else if(sourceClass %in% c("MULTILINESTRING")){

                data <- x
                st_geometry(data) <- NULL
                dataNames <- names(data)
                fids <- lapply(unique(theCoords[,4]), function(i){
                  temp <- theCoords[which(theCoords[,4] == i),]
                  unique(temp[,3])
                })
                new <- tibble(fid = seq_along(unlist(fids)),
                              gid = rep(seq_along(fids), lengths(fids)))
                data <- tibble(rep(data[,1], lengths(fids)))
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "gid", dataNames)

              } else if(sourceClass %in% c("POLYGON")){

                data <- x
                st_geometry(data) <- NULL
                dataNames <- names(data)
                fids <- unique(theCoords[, 4])
                new <- tibble(fid = fids, gid = fids)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "gid", dataNames)

              } else if(sourceClass %in% c("MULTIPOLYGON")){

                data <- x
                st_geometry(data) <- NULL
                dataNames <- colnames(data)
                fids <- lapply(unique(theCoords[,5]), function(i){
                  temp <- theCoords[which(theCoords[,5] == i),]
                  unique(temp[,4])
                })
                new <- tibble(fid = seq_along(unlist(fids)),
                              gid = rep(seq_along(fids), lengths(fids)))
                data <- as.data.frame(data[rep(seq_len(nrow(data)), lengths(fids)),])
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "gid", dataNames)

              }
            } else{
              # what happens if a sf-object has different feature-types?
            }

            return(out)
          }
)

# ppp ----
#' @rdname getTable
#' @examples
#'
#' getTable(gtPPP$...)
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getTable",
          signature = "ppp",
          definition = function(x){
            temp <- x
            out <- tibble(fid = seq_along(temp$x), gid = seq_along(temp$x))
            if("marks" %in% names(temp)){
              attr <- as_tibble(x = temp$marks)
              out <- bind_cols(out, attr)
            }
            return(out)
          }
)

# RasterLayer ----
#' @rdname getTable
#' @examples
#'
#' getTable(gtRasters$categorical)
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getTable",
          signature = "RasterLayer",
          definition = function(x){
            if(length(x@data@attributes) == 0){
              vals <- sort(unique(getValues(x = x)))
              tibble(fid = seq_along(vals), values = vals)
            } else{
              names <- names(x@data@attributes[[1]])
              names[which(names == "id")] <- "fid"
              out <- as_tibble(x@data@attributes[[1]])
              names(out) <- names
              return(out)
            }
          }
)

# RasterBrick ----
#' @rdname getTable
#' @export
setMethod(f = "getTable",
          signature = "RasterBrick",
          definition = function(x){

          }
)

# matrix ----
#' @rdname getTable
#' @export
setMethod(f = "getTable",
          signature = "matrix",
          definition = function(x){
            vals <- unique(x = as.vector(x))
            tibble(fid = seq_along(vals), values = vals)
          }
)