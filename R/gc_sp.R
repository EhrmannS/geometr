#' Make a geometry object of class \code{sp}
#' @param input the object from which to make an object of class \code{sp}.
#' @return an object of class \code{sp}
#' @family spatial classes
#' @examples
#' (spPoints <- gc_sp(input = gtGeoms$point))
#' (spLines <- gc_sp(input = gtGeoms$line))
#' (spPolygon <- gc_sp(input = gtGeoms$polygon))
#' @name gc_sp
#' @rdname gc_sp
NULL

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

#' @rdname gc_sp
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertClass
#' @importFrom sp SpatialPoints SpatialPointsDataFrame Line Lines SpatialLines
#'   SpatialLinesDataFrame Polygon Polygons SpatialPolygons
#'   SpatialPolygonsDataFrame spTransform CRS
#' @export
setMethod(f = "gc_sp",
          signature = "geom",
          definition = function(input){

            theCoords <- getVertices(x = input)
            theData <- getTable(x = input)
            theCRS <- getCRS(x = input)
            bbox <- getExtent(x = input)
            theWindow = tibble(x = c(min(bbox$x), max(bbox$x), max(bbox$x), min(bbox$x), min(bbox$x)),
                               y = c(min(bbox$y), min(bbox$y), max(bbox$y), max(bbox$y), min(bbox$y)))

            featureType <- input@type

            if(featureType %in% c("point")){

              temp <- theCoords[c("x", "y")]
              out <- SpatialPoints(temp)
              # if there are columns in the geoms attribute table, make a Spatial*DataFrame
              if(!all(names(theData) %in% c("fid", "gid"))){
                attr <- theData[,!names(theData) %in% c("fid", "gid")]
                out <- SpatialPointsDataFrame(out, data = attr, match.ID = FALSE)
              }

            } else if(featureType %in% c("line")){

              fids <- unique(theData$fid)
              tempOut <- list()
              outLines <- list()

              for(i in seq_along(fids)){
                tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% i,]
                outLines <- c(outLines, Lines(list(Line(tempVerts)), fids[i]))
              }
              out <- SpatialLines(outLines)

              # if there are columns in the geoms attribute table, make a Spatial*DataFrame
              if(!all(names(theData) %in% c("fid", "gid"))){
                attr <- theData[,!names(theData) %in% c("fid", "gid")]
                out <- SpatialLinesDataFrame(out, data = attr, match.ID = FALSE)
              }

            } else if(featureType %in% c("polygon")){

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
                attr <- unique(theData[,!names(theData) %in% c("fid", "gid")])
                out <- SpatialPolygonsDataFrame(out, data = attr, match.ID = FALSE)
              }
            }
            if(!is.na(theCRS)){
              out <- spTransform(x = out, CRSobj = CRS(theCRS))
            }

            return(out)
          }
)