#' Get the attribute table of a spatial object.
#' @param x the object from which to derive the attribute table.
#' @name getTable
#' @rdname getTable
NULL

#' @rdname getTable
#' @export
if(!isGeneric("getTable")){
  setGeneric(name = "getTable",
             def = function(x, ...){
               standardGeneric("getTable")
             }
  )
}

#' @rdname getTable
#' @examples
#' # the attribute table of ...
#'
#' # ... a geom
#' getTable(gtGeoms$mask)
#'
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getTable",
          signature = "geom",
          definition = function(x){
            as_tibble(x@attr)
          }
)

#' @rdname getTable
#' @examples
#' # ... any Spatial* object
#' getTable(gtSP$SpatialPolygons)
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getTable",
          signature = signature("Spatial"),
          definition = function(x){

            theData <- NULL
            sourceClass <- class(x)[1]
            prev <- 0

            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialPixels", "SpatialPixelsDataFrame")){
              type <- "point"

              if(sourceClass %in% "SpatialPointsDataFrame"){
                theData <- tibble(fid = seq_along(x@coords[,1]), n = 1)
                theData <- bind_cols(theData, x@data)
              } else{
                theData <- tibble(fid = seq_along(x@coords[,1]), n = 1)
              }

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
              type <- "point"

              for(i in seq_along(x@coords)){

                if(sourceClass %in% "SpatialMultiPointsDataFrame"){
                  tempData <- tibble(i, length(x@coords[[i]][,1]), x@data[i,])
                  theData <- bind_rows(theData, tempData)
                  otherNames <- colnames(x@data)
                } else{
                  tempData <- tibble(i, length(x@coords[[i]][,1]))
                  theData <- bind_rows(theData, tempData)
                  otherNames <- NULL
                }
              }
              colnames(theData) <- c("fid", "n", otherNames)

            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
              type <- "line"

              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]

                for(j in seq_along(theLines@Lines)){
                  if(sourceClass %in% "SpatialLinesDataFrame"){
                    tempData <- tibble(prev + j, dim(theLines@Lines[[j]]@coords)[1], x@data[i,])
                    theData <- bind_rows(theData, tempData)
                    otherNames <- colnames(x@data)
                  } else{
                    theData <- bind_rows(theData, tibble(prev + j, dim(theLines@Lines[[j]]@coords)[1]))
                    otherNames <- NULL
                  }
                }
                prev <- prev + length(theLines@Lines)

              }
              colnames(theData) <- c("fid", "n", otherNames)

            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialGrid", "SpatialGridDataFrame")){
              type <- "polygon"

              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]
                prev <- 0

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
                  prev <- prev + dim(polyCoords)[1]
                }
                tempData <- bind_cols(tibble(i, prev), tempData)

                theData <- bind_rows(theData, tempData)

              }
              colnames(theData) <- c("fid", "n", otherNames)

            }
            return(theData)
          })

#' @rdname getTable
#' @examples
#' # ... an sf object
#' getTable(gtSF$multiline)
#' @importFrom tibble tibble as_tibble
#' @importFrom sf st_geometry<-
#' @export
setMethod(f = "getTable",
          signature = "sf",
          definition = function(x){

            sourceClass <- st_geometry_type(x)
            theCoords <- st_coordinates(x)
            # if(dim(theCoords)[1] > 1){
            #   theCoords <- theCoords[!duplicated(theCoords),]
            # }
            if(length(unique(sourceClass)) == 1){
              sourceClass <- unique(sourceClass)
              if(sourceClass %in% c("POINT")){

                data <- x
                st_geometry(data) <- NULL
                fids <- seq_along(theCoords[, 1])

                new <- tibble(fid = fids, nos = 1)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "n", names(data))

              } else if(sourceClass %in% c("MULTIPOINT", "LINESTRING")){

                data <- x
                st_geometry(data) <- NULL
                fids <- unique(theCoords[, 3])
                nos <- unlist(lapply(fids, function(i){
                  length(which(theCoords[, 3] == i))
                }))
                new <- tibble(fids, nos)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "n", names(data))

              } else if(sourceClass %in% c("MULTILINESTRING", "POLYGON")){

                data <- x
                st_geometry(data) <- NULL
                fids <- unique(theCoords[, 4])
                nos <- unlist(lapply(fids, function(i){
                  length(which(theCoords[, 4] == i))
                }))
                new <- tibble(fids, nos)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "n", names(data))

              } else if(sourceClass %in% c("MULTIPOLYGON")){

                # check whether this is also required for the other types
                data <- x
                st_geometry(data) <- NULL
                dataName <- colnames(data)
                fids <- unique(theCoords[,5])
                nos <- lapply(fids, function(i){
                  temp <- theCoords[which(theCoords[,5] == i),]

                  lapply(unique(temp[,4]), function(j){
                    length(which(temp[,4] == j))
                  })
                })

                data <- data[rep(row.names(data), lengths(nos)),]
                data <- as_tibble(data)

                nos <- unlist(nos)
                fids <- seq_along(nos)

                new <- tibble(fids, nos)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "n", dataName)
              }
            } else{
              # what happens if a sf-object has different feature-types?
            }

            return(out)
          }
)

#' @rdname getTable
#' @examples
#'
#' # ... a RasterLayer
#' getTable(gtRasters$categorical)
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getTable",
          signature = "RasterLayer",
          definition = function(x){
            if(length(x@data@attributes) == 0){
              tibble()
            } else{
              as_tibble(x@data@attributes[[1]])
            }
          }
)