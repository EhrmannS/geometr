#' Get the table of coordinates of a spatial object.
#'
#' @param x the object from which to extract the coordinates
#' @return A table of the coordinates \code{x} is made up of.
#' @examples
#' getVertices(gtGeoms$polygon)
#' @family getters
#' @name getVertices
#' @rdname getVertices
NULL

# generic ----
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

# geom ----
#' @rdname getVertices
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getVertices",
          signature = "geom",
          definition = function(x){
            as_tibble(x@vert)
          }
)

# Spatial ----
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

              theCoords <- bind_cols(as_tibble(x@coords),
                                     fid = seq_along(x@coords[,1]))
              colnames(theCoords) <- c("x", "y", "fid")

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){

              temp <- x
              nCoords <- 0
              for(i in seq_along(temp@coords)){
                tempCoords <- tibble(x = temp@coords[[i]][,1],
                                     y = temp@coords[[i]][,2])
                theCoords <- bind_rows(theCoords, tempCoords)
              }
              theCoords <- tibble(x = theCoords$x,
                                  y = theCoords$y,
                                  fid = seq_along(theCoords$x))

            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){

              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]
                for(j in seq_along(theLines@Lines)){
                  theLine <- theLines@Lines[[j]]

                  tempCoords <- tibble(x = theLine@coords[,1],
                                       y = theLine@coords[,2],
                                       fid = prev + j)
                  theCoords <- bind_rows(theCoords, tempCoords)
                }
                prev <- prev + length(theLines@Lines)
              }

            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){

              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]
                for(j in seq_along(thePolys@Polygons)){
                  thePoly <- thePolys@Polygons[[j]]
                  polyCoords <- thePoly@coords

                  tempCoords <- tibble(x = polyCoords[,1],
                                       y = polyCoords[,2],
                                       fid = i)
                  theCoords <- bind_rows(theCoords, tempCoords)
                }
              }

            }

            return(theCoords)
          }
)

# sf ----
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

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = seq_along(theCoords[, 1]))

              } else if(sourceClass %in% c("MULTIPOINT")){

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = seq_along(theCoords[, 1]))

              } else if(sourceClass %in% c("LINESTRING")){

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = theCoords[,3])

              } else if(sourceClass %in% c("MULTILINESTRING")){

                vids <- lapply(unique(theCoords[,4]), function(i){
                  temp <- theCoords[which(theCoords[,4] == i),]

                  lapply(unique(temp[,3]), function(j){
                    seq_along(which(temp[,3] == j))
                  })
                })
                vids <- unlist(vids, recursive = FALSE)
                fids <- rep(1:length(vids), lengths(vids))

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = fids)

              } else if(sourceClass %in% c("POLYGON")){

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = theCoords[,4])

              } else if(sourceClass %in% c("MULTIPOLYGON")){

                vids <- lapply(unique(theCoords[,5]), function(i){
                  temp <- theCoords[which(theCoords[,5] == i),]

                  lapply(unique(temp[,4]), function(j){
                    seq_along(which(temp[,4] == j))
                  })
                })
                vids <- unlist(vids, recursive = FALSE)
                fids <- rep(1:length(vids), lengths(vids))

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = fids)

              }
            } else{
              # what happens if a sf-object has different feature-types?
              stop("simple features with multiple feature types are not yet supported.")
            }

            return(theCoords)
          }
)

# ppp ----
#' @rdname getVertices
#' @examples
#'
#' getVertices(gtPPP)
#' @export
setMethod(f = "getVertices",
          signature = "ppp",
          definition = function(x){
            bla <- x
            tibble(x = bla$x,
                   y = bla$y,
                   fid = seq_along(bla$x))
          }
)