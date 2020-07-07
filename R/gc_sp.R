#' Transform a spatial object to class \code{Spatial}
#'
#' @param input the object to transform to class \code{Spatial}.
#' @return an object of class \code{Spatial}
#' @family spatial classes
#' @examples
#' gc_sp(input = gtGeoms$point)
#'
#' gc_sp(input = gtGeoms$line)
#'
#' gc_sp(input = gtGeoms$polygon)
#' @name gc_sp
#' @rdname gc_sp
NULL

# generic ----
#' @rdname gc_sp
#' @name gc_sp
#' @export
if(!isGeneric("gc_sp")){
  setGeneric(name = "gc_sp",
             def = function(input, ...){
               standardGeneric("gc_sp")
             }
  )
}

# geom ----
#' @rdname gc_sp
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertClass
#' @importFrom dplyr left_join
#' @importFrom sp SpatialPoints SpatialPointsDataFrame Line Lines SpatialLines
#'   SpatialLinesDataFrame Polygon Polygons SpatialPolygons
#'   SpatialPolygonsDataFrame proj4string<- CRS
#' @export
setMethod(f = "gc_sp",
          signature = "geom",
          definition = function(input = NULL){

            theCoords <- getPoints(x = input)
            theData <- getFeatures(x = input)
            theGroups <- getGroups(x = input)
            theCRS <- getCRS(x = input)
            featureType <- getType(input)[2]

            makeDF <- FALSE

            if(featureType == "point"){
              attr <- tibble(fid = theCoords$fid)

              temp <- theCoords[c("x", "y")]
              out <- SpatialPoints(temp)

              if(!all(names(theCoords) %in% c("x", "y", "fid"))){
                makeDF <- TRUE
                attr <- theCoords[,!names(theCoords) %in% c("x", "y")]
              }
              if(!all(names(theData) %in% c("fid", "gid"))){
                makeDF <- TRUE
                temp <- theData[,!names(theData) %in% c("gid")]
                attr <- left_join(x = attr, y = temp, by = "fid")
              }
              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
                temp <- left_join(x = theData[c("fid", "gid")], y = theGroups, by = "gid")
                attr <- left_join(x = attr, y = temp, by = "fid")
              }

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- SpatialPointsDataFrame(out, data = attr, match.ID = FALSE)
              }

            } else if(featureType == "line"){
              attr <- tibble(fid = theData$fid)

              fids <- unique(theData$fid)
              tempOut <- list()
              outLines <- list()

              for(i in seq_along(fids)){
                tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% i,]
                outLines <- c(outLines, Lines(list(Line(tempVerts)), fids[i]))
              }
              out <- SpatialLines(outLines)

              if(!all(names(theData) %in% c("fid", "gid"))){
                makeDF <- TRUE
                temp <- theData[,!names(theData) %in% c("gid")]
                attr <- left_join(x = attr, y = temp, by = "fid")
              }
              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
                temp <- left_join(x = theData[c("fid", "gid")], y = theGroups, by = "gid")
                attr <- left_join(x = attr, y = temp, by = "fid")
              }

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- SpatialLinesDataFrame(out, data = attr, match.ID = FALSE)
              }

            } else if(featureType == "polygon"){
              attr <- tibble(fid = theData$fid)

              gids <- unique(theData$gid)
              outPolygons <- list()
              for(i in seq_along(gids)){

                tempFids <- theData$fid[theData$gid == gids[i]]
                tempPolys <- list()
                for(j in seq_along(tempFids)){
                  tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids[j],]
                  dups <- as.numeric(duplicated(tempVerts))
                  dups <- c(0, dups[-length(dups)])
                  rings <- 1 + cumsum(dups)
                  temp <- split(x = tempVerts, f = rings)
                  tempVerts <- lapply(seq_along(temp), function(x){
                    newVerts <- temp[[x]]
                    if(min(newVerts$x) > min(tempVerts$x) & max(newVerts$x) < max(tempVerts$x) &
                       min(newVerts$y) > min(tempVerts$y) & max(newVerts$y < max(tempVerts$y))){
                      Polygon(as.matrix(newVerts), hole = TRUE)
                    } else{
                      Polygon(as.matrix(newVerts))
                    }
                  })
                  tempPolys <- c(tempPolys, tempVerts)
                }
                outPolygons <- c(outPolygons, Polygons(tempPolys, gids[i]))

              }
              # make a SpatialPolygon out of that
              out <- SpatialPolygons(outPolygons)

              if(!all(names(theData) %in% c("fid", "gid"))){
                makeDF <- TRUE
                temp <- theData[,!names(theData) %in% c("gid")]
                attr <- left_join(x = attr, y = temp, by = "fid")
              }
              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
                temp <- left_join(x = theData[c("fid", "gid")], y = theGroups, by = "gid")
                attr <- left_join(x = attr, y = temp, by = "fid")
              }

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- SpatialPolygonsDataFrame(out, data = attr, match.ID = FALSE)
              }
            } else if(featureType == "grid"){

            }

            if(!is.na(theCRS)){
              proj4string(out) <- CRS(theCRS)
            }

            return(out)
          }
)
