#' Get the table of feature attributes
#'
#' Get tabular information of the attributes of features.
#' @param x the object from which to derive the attribute table.
#' @param ... subset based on logical predicates defined in terms of the
#'   columns in \code{x} or a vector of booleans. Multiple conditions are
#'   combined with \code{&}. Only rows where the condition evaluates to TRUE are kept.
#' @return A table of the feature attributes of \code{x} or an object where the
#'   features table has been subsetted.
#' @family getters
#' @examples
#' getFeatures(x = gtGeoms$polygon)
#'
#' # get a subset of an sf-object
#' getFeatures(x = gtSF$multilinestring, a == 1)
#'
#' # get the values of a RasterLayer
#' getFeatures(x = gtRasters$continuous)
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
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getFeatures",
          signature = "geom",
          definition = function(x, ...){

            theType <- getType(x = x)[2]

            if(length(exprs(...)) > 0){

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

              out <- new(Class = "geom",
                         type = theType,
                         point = thePoints,
                         feature = list(geometry = theFeatures),
                         group = list(geometry = theGroups),
                         window = getWindow(x = x),
                         scale = "absolute",
                         crs = getCRS(x = x),
                         history = getHistory(x = x))

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
                    tempFeatures <- tibble(fid = seq_along(temp), gid = temp, values = temp)
                  } else {
                    theInput <- unlist(theInput, use.names = FALSE)
                    tempFeatures <- tibble(fid = 1:length(theInput), gid = theInput, values = theInput)
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
#' @importFrom methods as
#' @importFrom tibble tibble enframe
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
                    tempData <- enframe(x@data[i,], name = NULL)
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
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr summarise group_by n_distinct
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

                  fact <- 10**nchar(max(theCoords[,3]))
                  toSeq <- theCoords[,4]*fact + theCoords[,3]
                  toSeq <- rle(toSeq)
                  fids <- seq_along(toSeq$values)
                  gids <- summarise(group_by(as_tibble(theCoords), L2), count = n_distinct(L1))

                  new <- tibble(fid = fids,
                                gid = rep(seq_along(gids$L2), gids$count))
                  data <- tibble(rep(data[,1], gids$count))
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

                  fact <- 10**nchar(max(theCoords[,4]))
                  toSeq <- theCoords[,5]*fact + theCoords[,4]
                  toSeq <- rle(toSeq)
                  fids <- seq_along(toSeq$values)
                  gids <- summarise(group_by(as_tibble(theCoords), L3), count = n_distinct(L2))

                  new <- tibble(fid = fids,
                                gid = rep(seq_along(gids$L3), gids$count))
                  data <- as.data.frame(data[rep(seq_len(nrow(data)), gids$count),])
                  out <- bind_cols(new, data)
                  colnames(out) <- c("fid", "gid", dataNames)

                }
              } else{
                # what happens if a sf-object has different feature-types?
                stop("simple features with multiple feature types are not yet supported.")
              }

            }

            return(out)
          }
)

# ppp ----
#' @rdname getFeatures
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

            temp <- as.vector(t(x))
            out <- tibble(fid = seq_along(temp), gid = seq_along(temp), values = temp)
            return(out)
          }
)