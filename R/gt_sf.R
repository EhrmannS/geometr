#' Transform a \code{geom} to and from an \code{sf} object
#'
#' An object of class \code{sf} is the sf-package representation of a
#' \code{geom}.
#' @param input [\code{geom} | \code{sf}]\cr Object to transform.
#' @return If \code{input} is a \code{geom} and has attributes other than
#'   \code{fid} and \code{gid}, a "Simple feature collection", otherwise a
#'   "Geometry set". Several features of the \code{geom} are returned as MULTI*
#'   feature, when they have \code{gid} and optionally other attributes in
#'   common, otherwise they are returned as a single simple feature.
#'
#'   If \code{input} is a \code{sf}, a \code{geom} that reflects the simple
#'   feature type.
#' @family geometry tools
#' @examples
#' (sfPoints <- gt_sf(input = gtGeoms$point))
#' (sfLines <- gt_sf(input = gtGeoms$line))
#' (sfPolygon <- gt_sf(input = gtGeoms$polygon))
#'
#' sfPoly <- gtSF$polygon
#' plot(sfPoly)
#' visualise(geom = gt_sf(input = sfPoly))
#' @importFrom checkmate assertClass
#' @importFrom raster crs
#' @importFrom sf st_multipoint st_point st_multilinestring st_linestring st_sfc
#'   st_sf st_multipolygon st_polygon st_set_crs
#' @export

gt_sf <- function(input = NULL){

  isGeom <- testClass(input, classes = "geom")
  isSf <- testClass(x = input, classes = "sf")
  assert(isGeom, isSf, .var.name = "input")

  theCoords <- getVertices(x = input)
  theData <- getTable(x = input)
  theCRS <- getCRS(x = input)
  bbox <- getExtent(x = input)
  theWindow <- tibble(x = rep(c(bbox$x), each = 2),
                      y = c(bbox$y, rev(bbox$y)))

  if(isSf){

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
    history <- paste0("geometry was created from an sf-object of geometry type '", sourceClass, "'")
    theCRS <- getCRS(x = input)

    out <- new(Class = "geom",
               type = type,
               vert = theCoords,
               attr = theData,
               window = theWindow,
               scale = "absolute",
               crs = theCRS,
               history = list(history))

  } else {
    featureType <- input@type

    if(featureType %in% c("point")){

      gids <- unique(theData$gid)
      tempOut <- list()
      for(i in seq_along(gids)){
        tempFids <- theData$fid[theData$gid == gids[i]]
        tempVerts <- theCoords[c("x", "y")][theCoords$fid %in% tempFids,]

        if(length(tempFids) > 1){
          # make MULTIPOINT
          # assert that there are no duplicate coordinates for it to be a simple feature
          theCoords <- theCoords[!duplicated(theCoords[c("x", "y")]),]
          tempOut <- c(tempOut, list(st_multipoint(as.matrix(tempVerts))))
        } else{
          # make POINT
          tempOut <- c(tempOut, list(st_point(as.matrix(tempVerts))))
        }
      }
      out <- st_sfc(tempOut)

      if(!all(names(theData) %in% c("fid", "gid"))){
        attr <- unique(theData[,!names(theData) %in% c("fid")])
        if(length(out) < dim(attr)[1]){
          stop("MULTIPOINTS don't support individual attributes per point.")
        }
        out <- st_sf(geom = out, attr[,!names(attr) %in% c("gid")])
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
          # make LINE
          tempOut <- c(tempOut, list(st_linestring(as.matrix(tempVerts))))

        }
      }
      out <- st_sfc(tempOut)

      if(!all(names(theData) %in% c("fid", "gid"))){
        attr <- unique(theData[,!names(theData) %in% c("fid")])
        if(length(out) < dim(attr)[1]){
          stop("MULTILINESTRING doesn't support individual attributes per line.")
        }
        out <- st_sf(geom = out, attr[,!names(attr) %in% c("gid")])
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

      if(!all(names(theData) %in% c("fid", "gid"))){
        attr <- unique(theData[,!names(theData) %in% c("fid")])

        if(length(out) < dim(attr)[1]){
          stop("MULTIPOLYGON doesn't support individual attributes per polygon.")
        }
        out <- st_sf(geom = out, attr[,!names(attr) %in% c("gid")])
      }
    }
    out <- st_set_crs(x = out, value = theCRS)

  }


  return(out)
}