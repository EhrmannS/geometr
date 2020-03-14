#' Transform a spatial object to class \code{sf}
#'
#' @param input the object to transform to class \code{sf}.
#' @return If \code{input} is a \code{geom} and has attributes other than
#'   \code{fid} and \code{gid}, a "Simple feature collection", otherwise a
#'   "Geometry set". Several features of the \code{geom} are returned as MULTI*
#'   feature, when they have \code{gid} and optionally other attributes in
#'   common, otherwise they are returned as a single simple feature.
#' @family spatial classes
#' @examples
#' gc_sf(input = gtGeoms$point)
#'
#' gc_sf(input = gtGeoms$line)
#'
#' gc_sf(input = gtGeoms$polygon)
#' @importFrom checkmate assertClass
#' @importFrom raster crs
#' @importFrom sf st_multipoint st_point st_multilinestring st_linestring st_sfc
#'   st_sf st_multipolygon st_polygon st_set_crs
#' @name gc_sf
#' @rdname gc_sf
NULL

# generic ----
#' @rdname gc_sf
#' @name gc_sf
#' @export
if(!isGeneric("gc_sf")){
  setGeneric(name = "gc_sf",
             def = function(input, ...){
               standardGeneric("gc_sf")
             }
  )
}

# geom ----
#' @rdname gc_sf
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "gc_sf",
          signature = "geom",
          definition = function(input = NULL){

            theCoords <- getPoints(x = input)
            theData <- getFeatures(x = input)
            theGroups <- getGroups(x = input)
            theCRS <- getCRS(x = input)
            featureType <- getType(input)[2]

            makeDF <- FALSE

            if(featureType %in% c("point")){

              gids <- unique(theData$gid)
              tempOut <- list()
              for(i in seq_along(gids)){
                tempFids <- theData$fid[theData$gid == gids[i]]
                tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids,]

                if(length(tempVerts$x) > 1){
                  # make MULTIPOINT
                  # ensure that there are no duplicate coordinates for it to be a simple feature
                  tempVerts <- tempVerts[!duplicated(tempVerts[c("x", "y")]),]
                  tempOut <- c(tempOut, list(st_multipoint(as.matrix(tempVerts))))
                } else{
                  # make POINT
                  tempOut <- c(tempOut, list(st_point(as.matrix(tempVerts))))
                }
              }
              out <- st_sfc(tempOut)

              attr <- tibble(fid = unique(theCoords$fid))
              if(!all(names(theCoords) %in% c("x", "y", "fid"))){
                if(length(out) < dim(theCoords)[1]){
                  warning("MULTIPOINTS don't support individual attributes per point, ignoring '", names(theCoords)[!names(theCoords) %in% c("x", "y", "fid")] , "'.")
                } else {
                  makeDF <- TRUE
                  attr <- theCoords[,!names(theCoords) %in% c("x", "y")]
                }
              }

              if(!all(names(theData) %in% c("fid", "gid"))){
                makeDF <- TRUE
              }
              attr <- merge(x = attr, y = theData, by = "fid", all.x = TRUE, suffixes = c(".point", ".feature"))

              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
              }
              attr <- as_tibble(merge(x = attr, y = theGroups, by = "gid", all.x = TRUE, suffixes = c(".feature", ".group")))

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- st_sf(geom = out, attr)
              } else {
                out <- st_sf(geom = out)
              }

            } else if(featureType %in% c("line")){

              gids <- unique(theData$gid)
              tempOut <- list()
              for(i in seq_along(gids)){
                tempFids <- theData$fid[theData$gid == gids[i]]

                if(length(tempFids) > 1){
                  # make MULTILINESTRING
                  subStrings <- list()
                  for(j in seq_along(tempFids)){
                    tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids[j],]
                    subStrings <- c(subStrings, list(as.matrix(tempVerts)))
                  }
                  tempOut <- c(tempOut, list(st_multilinestring(subStrings)))

                } else{
                  tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids,]
                  # make LINESTRING
                  tempOut <- c(tempOut, list(st_linestring(as.matrix(tempVerts))))

                }
              }
              out <- st_sfc(tempOut)

              attr <- tibble(gid = unique(theData$gid))
              if(!all(names(theData) %in% c("fid", "gid"))){
                uniqueData <- theData[,!names(theData) %in% c("fid")]
                uniqueData <- uniqueData[!duplicated(uniqueData),]
                if(length(out) != dim(uniqueData)[1]){
                  warning("MULTILINESTRING doesn't support individual attributes per line, ignoring '", names(theData)[!names(theData) %in% c("fid", "gid")] , "'.")
                } else {
                  makeDF <- TRUE
                  attr <- uniqueData
                }
              }

              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
              }
              attr <- as_tibble(merge(x = attr, y = theGroups, by = "gid", all.x = TRUE, suffixes = c(".feature", ".group")))

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- st_sf(geom = out, attr)
              } else {
                out <- st_sf(geom = out)
              }

            } else if(featureType %in% c("polygon")){

              gids <- unique(theData$gid)
              tempOut <- list()
              for(i in seq_along(gids)){

                tempFids <- theData$fid[theData$gid == gids[i]]

                if(length(tempFids) > 1){
                  # make MULTIPOLYGON
                  subPolys <- list()
                  for(j in seq_along(tempFids)){
                    tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids[j],]
                    dups <- as.numeric(duplicated(tempVerts))
                    dups <- c(0, dups[-length(dups)])
                    rings <- 1 + cumsum(dups)
                    tempVerts <- split(x = tempVerts, f = rings)
                    tempVerts <- lapply(seq_along(tempVerts), function(x){
                      as.matrix(tempVerts[[x]])
                    })
                    subPolys <- c(subPolys, list(tempVerts))
                  }
                  tempOut <- c(tempOut, list(st_multipolygon(subPolys)))

                } else{
                  # make POLYGON
                  tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids,]
                  dups <- as.numeric(duplicated(tempVerts))
                  dups <- c(0, dups[-length(dups)])
                  rings <- 1 + cumsum(dups)
                  tempVerts <- split(x = tempVerts, f = rings)
                  tempVerts <- lapply(seq_along(tempVerts), function(x){
                    as.matrix(tempVerts[[x]])
                  })
                  tempOut <- c(tempOut, list(st_polygon(tempVerts)))
                }
              }
              out <- st_sfc(tempOut)

              attr <- tibble(gid = unique(theData$gid))
              if(!all(names(theData) %in% c("fid", "gid"))){
                uniqueData <- theData[,!names(theData) %in% c("fid")]
                uniqueData <- uniqueData[!duplicated(uniqueData),]
                if(length(out) != dim(uniqueData)[1]){
                  warning("MULTIPOLYGON doesn't support individual attributes per polygon, ignoring '", paste0(names(theData)[!names(theData) %in% c("fid", "gid")], collapse = ", ") , "'.")
                } else {
                  makeDF <- TRUE
                  attr <- uniqueData
                }
              }

              if(!all(names(theGroups) %in% c("gid"))){
                makeDF <- TRUE
              }
              attr <- as_tibble(merge(x = attr, y = theGroups, by = "gid", all.x = TRUE, suffixes = c(".feature", ".group")))

              if(makeDF){
                attr <- attr[,!names(attr) %in% c("fid", "gid")]
                out <- st_sf(geom = out, attr)
              } else {
                out <- st_sf(geom = out)
              }
            }
            out <- st_set_crs(x = out, value = theCRS)

            return(out)
          }
)
