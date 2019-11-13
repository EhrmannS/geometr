#' Get the table of feature attributes
#'
#' Get tabular information of the attributes of features.
#' @param x the object from which to derive the attribute table.
#' @param ... subset based on logical predicates defined in terms of the
#'   variables in \code{x} or a vector of booleans. Multiple conditions are
#'   combined with &. Only rows where the condition evaluates to TRUE are kept.
#' @return A table of the feature attributes of \code{x} or an object where the
#'   features table has been subsetted.
#' @family getters
#' @name getFeatures
#' @rdname getFeatures
NULL

# generic ----
#' @rdname getFeatures
#' @name getFeatures
#' @export
if(!isGeneric("getFeatures")){
  setGeneric(name = "getFeatures",
             def = function(x, ...){
               standardGeneric("getFeatures")
             }
  )
}

# any ----
#' @rdname getFeatures
#' @export
setMethod(f = "getFeatures",
          signature = "ANY",
          definition = function(x, ...){
            NULL
          }
)

# geom ----
#' @rdname getFeatures
#' @examples
#' getFeatures(x = gtGeoms$polygon)
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getFeatures",
          signature = "geom",
          definition = function(x, ...){

            theType <- getType(x = x)[2]

            if(length(exprs(...)) > 0){
              out <- x
              subset <- enquos(...)
              isLogical <- tryCatch(is.logical(eval_tidy(expr = subset[[1]])), error = function(e) FALSE)
              thePoints <- getPoints(x = x)
              theFeatures <- getFeatures(x = x)
              theGroups <- getGroups(x = x)
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = theFeatures)
              }
              theFeatures <- theFeatures[matches,]
              thePoints <- thePoints[thePoints$fid %in% theFeatures$fid,]
              theGroups <- theGroups[theGroups$gid %in% theFeatures$gid,]

              out@point <- thePoints
              out@feature <- list(geometry = theFeatures)
              out@group <- list(geometry = theGroups)
            } else {
              if(theType == "grid"){
                theFeatures <- x@feature
                out <- list()
                for(i in seq_along(theFeatures)){
                  theInput <- theFeatures[[i]]
                  theName <- names(theFeatures)[i]

                  if(all(names(theInput) %in% c("val", "len"))){
                    temp <- list(lengths = theInput$len,
                                 values = theInput$val)
                    attr(temp, "class") <- "rle"
                    temp <- inverse.rle(temp)
                    tempFeatures <- tibble(fid = seq_along(temp), gid = seq_along(temp), values = temp)
                  } else {
                    tempFeatures <- as_tibble(cbind(fid = 1:dim(theInput)[1], gid = 1:dim(theInput)[1], theInput))
                  }

                  if(length(theFeatures) > 1){
                    out <- c(out, setNames(list(tempFeatures), theName))
                  } else {
                    out <- tempFeatures
                  }
                }
              } else {
                out <- x@feature$geometry
              }

            }

            return(out)
          }
)

# Spatial ----
#' @rdname getFeatures
#' @examples
#'
#' getFeatures(x = gtSP$SpatialPolygons)
#' @importFrom methods as
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getFeatures",
          signature = signature("Spatial"),
          definition = function(x, ...){

            if(length(exprs(...)) > 0){

              out <- x
              subset <- enquos(...)
              isLogical <- tryCatch(is.logical(eval_tidy(expr = subset[[1]])), error = function(e) FALSE)
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = x@data)
              }
              out <- out[matches,]

            } else {

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
              out <- theData
            }

            return(out)
          }
)

# sf ----
#' @rdname getFeatures
#' @examples
#'
#' getFeatures(gtSF$multiline)
#' @importFrom tibble tibble as_tibble
#' @importFrom sf st_geometry_type st_coordinates st_geometry<-
#' @export
setMethod(f = "getFeatures",
          signature = "sf",
          definition = function(x, ...){

            if(length(exprs(...)) > 0){
              out <- x
              subset <- enquos(...)
              isLogical <- tryCatch(is.logical(eval_tidy(expr = subset[[1]])), error = function(e) FALSE)
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = x)
              }
              out <- out[matches,]
            } else {

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

            }

            return(out)
          }
)

# ppp ----
#' @rdname getFeatures
#' @examples
#'
#' getFeatures(gtPPP)
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "getFeatures",
          signature = "ppp",
          definition = function(x, ...){

            if(length(exprs(...)) > 0){
              out <- x
              subset <- enquos(...)
              isLogical <- tryCatch(is.logical(eval_tidy(expr = subset[[1]])), error = function(e) FALSE)
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = x)
              }
              out <- out[matches,]
            } else {
              temp <- x
              out <- tibble(fid = seq_along(temp$x), gid = seq_along(temp$x))
              if("marks" %in% names(temp)){
                out <- bind_cols(out, values = temp$marks)
              }
            }
            return(out)
          }
)

# Raster ----
#' @rdname getFeatures
#' @examples
#'
#' getFeatures(gtPPP)
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom raster getValues
#' @export
setMethod(f = "getFeatures",
          signature = "Raster",
          definition = function(x){

            temp <- getValues(x)
            out <- tibble(fid = seq_along(temp), gid = seq_along(temp), values = temp)
            return(out)
          }
)

# matrix ----
#' @rdname getFeatures
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getFeatures",
          signature = "matrix",
          definition = function(x){

            temp <- as.vector(x)
            out <- tibble(fid = seq_along(temp), gid = seq_along(temp), values = temp)
            return(out)
            return(out)
          }
)