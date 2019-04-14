#' Get the table of coordinates of a spatial object.
#' @param x the object from which to extract the coordinates
#' @examples
#' getVertices(gtGeoms$polygon)
#' @name getVertices
#' @rdname getVertices
NULL

#' @rdname getVertices
#' @name getVertices
#' @export
if(!isGeneric("getVertices")){
  setGeneric(name = "getVertices",
             def = function(x, ...){
               standardGeneric("getVertices")
             }
  )
}

#' @rdname getVertices
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getVertices",
          signature = "geom",
          definition = function(x){
            as_tibble(x@vert)
          }
)

#' @rdname getVertices
#' @examples
#'
#' getVertices(gtSP$SpatialPoints)
#' @importFrom methods as
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getVertices",
          signature = "Spatial",
          definition = function(x){
            theCoords <- NULL
            prev <- 0
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

            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame")){

              theCoords <- bind_cols(fid = seq_along(x@coords[,1]),
                                     vid = rep(1, length(x@coords[,1])),
                                     as_tibble(x@coords))
              colnames(theCoords) <- c("fid", "vid", "x", "y")

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){

              temp <- x
              nCoords <- 0
              for(i in seq_along(temp@coords)){
                tempCoords <- tibble(x = temp@coords[[i]][,1],
                                     y = temp@coords[[i]][,2])
                theCoords <- bind_rows(theCoords, tempCoords)
              }
              theCoords <- tibble(fid = seq_along(theCoords$x),
                                  vid = 1,
                                  x = theCoords$x,
                                  y = theCoords$y)

            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){

              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]
                for(j in seq_along(theLines@Lines)){
                  theLine <- theLines@Lines[[j]]

                  tempCoords <- tibble(fid = prev + j,
                                       vid = seq_along(theLine@coords[,1]),
                                       x = theLine@coords[,1],
                                       y = theLine@coords[,2])
                  theCoords <- bind_rows(theCoords, tempCoords)
                }
                prev <- prev + length(theLines@Lines)
              }

            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){

              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]
                prev <- 0
                for(j in seq_along(thePolys@Polygons)){
                  thePoly <- thePolys@Polygons[[j]]
                  polyCoords <- thePoly@coords

                  tempCoords <- tibble(fid = i,
                                       vid = prev + seq_along(polyCoords[,1]),
                                       x = polyCoords[,1],
                                       y = polyCoords[,2])
                  theCoords <- bind_rows(theCoords, tempCoords)
                  prev <- prev + dim(polyCoords)[1]
                }
              }

            }

            return(theCoords)
          }
)

#' @rdname getVertices
#' @examples
#'
#' getVertices(gtSF$multilinestring)
#' @importFrom tibble as_tibble
#' @importFrom sf st_geometry_type st_coordinates
#' @export
setMethod(f = "getVertices",
          signature = "sf",
          definition = function(x){

            sourceClass <- st_geometry_type(x)
            theCoords <- st_coordinates(x)
            if(length(unique(sourceClass)) == 1){
              sourceClass <- unique(sourceClass)
              if(sourceClass %in% c("POINT")){

                theCoords <- tibble(fid = seq_along(theCoords[, 1]),
                                    vid = 1,
                                    x = theCoords[,1],
                                    y = theCoords[,2])

              } else if(sourceClass %in% c("MULTIPOINT")){

                theCoords <- tibble(fid = seq_along(theCoords[, 1]),
                                    vid = 1,
                                    x = theCoords[,1],
                                    y = theCoords[,2])

              } else if(sourceClass %in% c("LINESTRING")){

                vids <- unique(theCoords[,3])
                vids <- unlist(lapply(seq_along(vids), function(x){
                  seq_along(which(theCoords[,3] == vids[x]))
                }))
                theCoords <- tibble(fid = theCoords[,3],
                                    vid = vids,
                                    x = theCoords[,1],
                                    y = theCoords[,2])

              } else if(sourceClass %in% c("MULTILINESTRING")){

                vids <- lapply(unique(theCoords[,4]), function(i){
                  temp <- theCoords[which(theCoords[,4] == i),]

                  lapply(unique(temp[,3]), function(j){
                    seq_along(which(temp[,3] == j))
                  })
                })
                grps <- rep(1:length(vids), lengths(vids))
                vids <- unlist(vids, recursive = FALSE)
                fids <- rep(1:length(vids), lengths(vids))
                grps <- rep(grps, lengths(vids))

                theCoords <- tibble(fid = fids,
                                    vid = unlist(vids),
                                    x = theCoords[,1],
                                    y = theCoords[,2])

              } else if(sourceClass %in% c("POLYGON")){

                vids <- unique(theCoords[,4])
                vids <- unlist(lapply(seq_along(vids), function(x){
                  seq_along(which(theCoords[,4] == vids[x]))
                }))
                theCoords <- tibble(fid = theCoords[,4],
                                    vid = vids,
                                    x = theCoords[,1],
                                    y = theCoords[,2])

              } else if(sourceClass %in% c("MULTIPOLYGON")){

                vids <- lapply(unique(theCoords[,5]), function(i){
                  temp <- theCoords[which(theCoords[,5] == i),]

                  lapply(unique(temp[,4]), function(j){
                    seq_along(which(temp[,4] == j))
                  })
                })
                grps <- rep(1:length(vids), lengths(vids))
                vids <- unlist(vids, recursive = FALSE)
                fids <- rep(1:length(vids), lengths(vids))
                grps <- rep(grps, lengths(vids))

                theCoords <- tibble(fid = fids,
                                    vid = unlist(vids),
                                    x = theCoords[,1],
                                    y = theCoords[,2])

              }
            } else{
              # what happens if a sf-object has different feature-types?
              stop("simple features with multiple feature types are not yet supported.")
            }

            return(theCoords)
          }
)