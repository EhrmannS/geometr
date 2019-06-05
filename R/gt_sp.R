#' Transform a \code{geom} to and from a \code{Spatial*} object
#'
#' An object of class \code{Spatial*} is the sp-package representation of a
#' \code{geom}.
#' @param input [\code{geom} | \code{sp}]\cr Object to transform.
#' @return Depending on \code{input} either a \code{geom} or a \code{sp}
#'   reflecting the type of \code{input}.
#' @family spatial classes
#' @examples
#' (spPoints <- gt_sp(input = gtGeoms$point))
#' (spLines <- gt_sp(input = gtGeoms$line))
#' (spPolygon <- gt_sp(input = gtGeoms$polygon))
#'
#' spPoly <- gtSP$SpatialPolygons
#' sp::plot(spPoly)
#' visualise(geom = gt_sp(input = spPoly))
#' @importFrom checkmate assertClass
#' @importFrom sp SpatialPoints SpatialPointsDataFrame Line Lines SpatialLines
#'   SpatialLinesDataFrame Polygon Polygons SpatialPolygons
#'   SpatialPolygonsDataFrame spTransform CRS
#' @export

gt_sp <- function(input = NULL){

  isGeom <- testClass(x = input, classes = "geom")
  isSp <- testClass(x = input, classes = "Spatial")
  assert(isGeom, isSp, .var.name = "input")

  theCoords <- getVertices(x = input)
  theData <- getTable(x = input)
  theCRS <- getCRS(x = input)
  bbox <- getExtent(x = input)
  theWindow <- tibble(x = rep(c(bbox$x), each = 2),
                      y = c(bbox$y, rev(bbox$y)))
  theCRS <- getCRS(x = input)

  if(isSp){

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
    history <- paste0("geometry was created from an object of class '", sourceClass, "'")

    out <- new(Class = "geom",
               type = type,
               vert = theCoords,
               feat = theData,
               group = tibble(gid = theData$gid),
               window = theWindow,
               scale = "absolute",
               crs = theCRS,
               history = list(history))

  } else{
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

  }



  return(out)
}