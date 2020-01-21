#' Get the table of point attributes
#'
#' Get tabular information of the attributes of points (incl. coordinates).
#' @param x the object from which to derive the attribute table.
#' @param ... subset based on logical predicates defined in terms of the
#'   columns in \code{x} or a vector of booleans. Multiple conditions are
#'   combined with \code{&}. Only rows where the condition evaluates to TRUE are kept.
#' @return A table of the point attributes of \code{x} or an object where the
#'   point table has been subsetted.
#' @examples
#' getPoints(x = gtGeoms$polygon)
#'
#' getPoints(x = gtGeoms$point)
#'
#' # for a raster object, the @point slot is extracted from its' compact storage
#' gtGeoms$grid$continuous@point
#' getPoints(x = gtGeoms$grid$continuous)
#' @family getters
#' @name getPoints
#' @rdname getPoints
NULL

# generic ----
#' @rdname getPoints
#' @name getPoints
#' @export
if(!isGeneric("getPoints")){
  setGeneric(name = "getPoints",
             def = function(x, ...){
               standardGeneric("getPoints")
             }
  )
}

# any ----
#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
          signature = "ANY",
          definition = function(x, ...){
            NULL
          }
)

# geom ----
#' @rdname getPoints
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getPoints",
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
                matches <- eval(parse(text = subset), envir = thePoints)
              }
              thePoints <- thePoints[matches,]
              theFeatures <- theFeatures[theFeatures$fid %in% thePoints$fid,]
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
                # rebuild points
                xGrid <- seq(from = x@point$x[1], length.out = x@point$x[2], by = x@point$x[3]) + 0.5
                yGrid <- seq(from = x@point$y[1], length.out = x@point$y[2], by = x@point$y[3]) + 0.5
                out <- tibble(fid = seq(1:(length(xGrid)*length(yGrid))),
                              x = rep(xGrid, times = length(yGrid)),
                              y = rep(yGrid, each = length(xGrid)))
              } else {
                out <- x@point
              }
            }

            return(out)
          }
)

# Spatial ----
#' @rdname getPoints
#' @importFrom methods as
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getPoints",
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
#' @rdname getPoints
#' @importFrom tibble as_tibble
#' @importFrom sf st_geometry_type st_coordinates
#' @export
setMethod(f = "getPoints",
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

                fact <- 10**nchar(max(theCoords[,3]))
                toSeq <- theCoords[,4]*fact + theCoords[,3]
                toSeq <- rle(toSeq)
                fids <- rep(seq_along(toSeq$values), toSeq$lengths)

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = fids)

              } else if(sourceClass %in% c("POLYGON")){

                theCoords <- tibble(x = theCoords[,1],
                                    y = theCoords[,2],
                                    fid = theCoords[,4])

              } else if(sourceClass %in% c("MULTIPOLYGON")){

                fact <- 10**nchar(max(theCoords[,4]))
                toSeq <- theCoords[,5]*fact + theCoords[,4]
                toSeq <- rle(toSeq)
                fids <- rep(seq_along(toSeq$values), toSeq$lengths)

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
#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
          signature = "ppp",
          definition = function(x){
            temp <- x
            tibble(x = temp$x,
                   y = temp$y,
                   fid = seq_along(temp$x))
          }
)

# Raster ----
#' @rdname getPoints
#' @importFrom tibble tibble as_tibble
#' @importFrom raster res
#' @export
setMethod(f = "getPoints",
          signature = "Raster",
          definition = function(x){

            res <- res(x)
            xGrid <- seq(from = x@extent@xmin, length.out = x@ncols, by = res[1]) + 0.5
            yGrid <- seq(from = x@extent@ymin, length.out = x@nrows, by = res[2]) + 0.5
            out <- tibble(x = rep(xGrid, times = length(yGrid)),
                          y = rep(yGrid, each = length(xGrid)),
                          fid = seq(1:(length(xGrid)*length(yGrid))))
            return(out)
          }
)

# matrix ----
#' @rdname getPoints
#' @export
setMethod(f = "getPoints",
          signature = "matrix",
          definition = function(x){

            xGrid <- seq(from = 0, length.out = ncol(x), by = 1) + 0.5
            yGrid <- seq(from = 0, length.out = nrow(x), by = 1) + 0.5
            out <- tibble(x = rep(xGrid, times = length(yGrid)),
                          y = rep(yGrid, each = length(xGrid)),
                          fid = seq(1:(length(xGrid)*length(yGrid))))
            return(out)

          }
)