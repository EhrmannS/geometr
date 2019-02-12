#' Determin number of vertices
#'
#' @param x [\code{geom}]\cr object from which to determine \code{length}.

setMethod(f = "length",
          signature = "geom",
          definition = function(x){
            dim(x@coords)[1]
          })

#' Print geom in the console
#'
#' @param object [\code{geom}]\cr object to \code{show}.

setMethod(f = "show",
          signature = "geom",
          definition = function(object){
            cat("class      : ", class(object), "\n", sep = "")
            cat("type       : ", object@type, "\n", sep = "")
            cat("features   : ", length(unique(object@coords$fid)), "  (", length(object), " vertices)\n", sep = "")
            cat("window     : ", min(object@window$x), ", ", max(object@window$x), ", ", min(object@window$y), ", ", max(object@window$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("extent     : ", min(object@coords$x), ", ", max(object@coords$x), ", ", min(object@coords$y), ", ", max(object@coords$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("scale      : ", object@scale, "\n", sep = "")
            cat("crs        : ", object@crs, "\n", sep = "")
            cat("attributes : ", length(object@attr), "  (", paste0(names(object@attr)[!names(object@attr) %in% c("x", "y")], collapse = ", "), ")\n", sep = "")
          })

#' Print rtTheme in the console
#'
#' @param object [\code{rtTheme}]\cr object to \code{show}.
#' @importFrom crayon green yellow red cyan
#' @importFrom cli symbol

setMethod(f = "show",
          signature = "rtTheme",
          definition = function(object){
            cat(ifelse(object@title$plot,
                       paste0(green(symbol$tick), yellow(" title    "), " in ", object@title$colour, " with fontsize ", object@title$fontsize),
                       paste0(red(symbol$cross), yellow(" title    "))), "\n")
            cat(ifelse(object@box$plot,
                       paste0(green(symbol$tick), yellow(" box      "), " in ", object@box$colour, " with ", object@box$linewidth, " wide ", object@box$linetype, " lines"),
                       paste0(red(symbol$cross), yellow(" box      "))),"\n")
            cat(ifelse(object@xAxis$plot,
                       paste0(green(symbol$tick), yellow(" xAxis    "), " with ", object@xAxis$bins, " bins and a margin of ", object@xAxis$margin, "\n",
                              ifelse(object@xAxis$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "'", object@xAxis$label$title, "' in ", object@xAxis$label$colour, " with fontsize ", object@xAxis$label$fontsize, ifelse(object@xAxis$label$rotation != 0, paste0(" and a rotation of ", object@xAxis$label$rotation), "")),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",
                              ifelse(object@xAxis$ticks$plot,
                                     paste0(green(symbol$tick), yellow("  - ticks  "), "in ", object@xAxis$ticks$colour, " with fontsize ", object@xAxis$ticks$fontsize, " rounded to ", object@xAxis$ticks$digits, ifelse(object@xAxis$ticks$digits == 1, " digit", " digits")),
                                     paste0(red(symbol$cross), yellow("  - ticks  ")))),
                       paste0(red(symbol$cross), yellow(" xAxis    "))), "\n")
            cat(ifelse(object@yAxis$plot,
                       paste0(green(symbol$tick), yellow(" yAxis    "), " with ", object@yAxis$bins, " bins and a margin of ", object@yAxis$margin, "\n",
                              ifelse(object@yAxis$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "'", object@yAxis$label$title, "' in ", object@yAxis$label$colour, " with fontsize ", object@yAxis$label$fontsize, ifelse(object@yAxis$label$rotation != 0, paste0(" and a rotation of ", object@yAxis$label$rotation), "")),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",
                              ifelse(object@yAxis$ticks$plot,
                                     paste0(green(symbol$tick), yellow("  - ticks  "), "in ", object@yAxis$ticks$colour, " with fontsize ", object@yAxis$ticks$fontsize, " rounded to ", object@yAxis$ticks$digits, ifelse(object@yAxis$ticks$digits == 1, " digit", " digits")),
                                     paste0(red(symbol$cross), yellow("  - ticks  ")))),
                       paste0(red(symbol$cross), yellow(" yAxis    "))), "\n")
            cat(ifelse(object@grid$plot,
                       paste0(green(symbol$tick), yellow(" grid     "), " in ", object@grid$colour, " with ", object@grid$linewidth, " wide ", object@grid$linetype, " lines"),
                       paste0(red(symbol$cross), yellow(" grid     "))), "\n")
            cat(ifelse(object@legend$plot,
                       paste0(green(symbol$tick), yellow(" legend   "), " with values ordered ", ifelse(object@legend$ascending, "ascending", "descending"), " in ", object@legend$bins, " bins and a relative height of ", object@legend$sizeRatio, "\n",
                              ifelse(object@legend$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "in ", object@legend$label$colour, " with fontsize ", object@legend$label$fontsize),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",
                              ifelse(object@legend$box$plot,
                                     paste0(green(symbol$tick), yellow("  - box    "), "in ", object@legend$box$colour, " with ", object@legend$box$linewidth, " wide ", object@legend$box$linetype, " lines"),
                                     paste0(red(symbol$cross), yellow("  - box    ")))),
                       paste0(red(symbol$cross), yellow(" legend    "))), "\n")
            cat(paste0(green(symbol$tick), yellow(" geom     "), " with ", object@geom$scale$x, "-colour scaled to ", cyan(object@geom$scale$to), ", ", object@geom$linewidth, " wide ", object@geom$linetype, " lines and ", object@geom$pointsize, " wide points of type ", object@geom$pointsymbol, "\n"))
            cat(paste0(green(symbol$tick), yellow(" raster   "), " with colours scaled to ", cyan(object@raster$scale)))
          })

#' @describeIn getTable get the attribute table of a \code{geom}
#' @importFrom tibble as_tibble
#' @examples
#' # the attribute table of ...
#'
#' # ... a geom
#' getTable(rtGeoms$mask)
#'
#' @export

setMethod(f = "getTable",
          signature = "geom",
          definition = function(x){
            as_tibble(x@attr)
          })

#' @describeIn getTable get the attribute table of any \code{Spatial*} object
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols
#' @examples
#' # ... a Spatial* object
#' getTable(rtSP$SpatialPolygons)
#'
#' @export

setMethod(f = "getTable",
          signature = "Spatial",
          definition = function(x){

            theData <- NULL
            sourceClass <- class(x)[1]
            prev <- 0

            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialPixels", "SpatialPixelsDataFrame")){
              type <- "point"

              if(sourceClass %in% "SpatialPointsDataFrame"){
                theData <- tibble(fid = seq_along(x@coords[,1]), n = 1)
                theData <- bind_cols(theData, x@data)
              } else{
                theData <- tibble(fid = seq_along(x@coords[,1]), n = 1)
              }

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
              type <- "point"

              for(i in seq_along(x@coords)){

                if(sourceClass %in% "SpatialMultiPointsDataFrame"){
                  tempData <- tibble(i, length(x@coords[[i]][,1]), x@data[i,])
                  theData <- bind_rows(theData, tempData)
                  otherNames <- colnames(x@data)
                } else{
                  tempData <- tibble(i, length(x@coords[[i]][,1]))
                  theData <- bind_rows(theData, tempData)
                  otherNames <- NULL
                }
              }
              colnames(theData) <- c("fid", "n", otherNames)

            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
              type <- "line"

              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]

                for(j in seq_along(theLines@Lines)){
                  if(sourceClass %in% "SpatialLinesDataFrame"){
                    tempData <- tibble(prev + j, dim(theLines@Lines[[j]]@coords)[1], x@data[i,])
                    theData <- bind_rows(theData, tempData)
                    otherNames <- colnames(x@data)
                  } else{
                    theData <- bind_rows(theData, tibble(prev + j, dim(theLines@Lines[[j]]@coords)[1]))
                    otherNames <- NULL
                  }
                }
                prev <- prev + length(theLines@Lines)

              }
              colnames(theData) <- c("fid", "n", otherNames)

            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialGrid", "SpatialGridDataFrame")){
              type <- "polygon"

              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]
                prev <- 0

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
                  prev <- prev + dim(polyCoords)[1]
                }
                tempData <- bind_cols(tibble(i, prev), tempData)

                theData <- bind_rows(theData, tempData)

              }
              colnames(theData) <- c("fid", "n", otherNames)

            }
            return(theData)
          })

#' @describeIn getTable get the attribute table of a \code{sf} object
#' @importFrom tibble tibble as_tibble
#' @examples
#' # ... an sf* object
#' getTable(rtSF$multiline)
#' @export

setMethod(f = "getTable",
          signature = "sf",
          definition = function(x){

            sourceClass <- st_geometry_type(x)
            theCoords <- st_coordinates(x)
            # if(dim(theCoords)[1] > 1){
            #   theCoords <- theCoords[!duplicated(theCoords),]
            # }
            if(length(unique(sourceClass)) == 1){
              sourceClass <- unique(sourceClass)
              if(sourceClass %in% c("POINT")){

                data <- x
                st_geometry(data) <- NULL
                fids <- seq_along(theCoords[, 1])

                new <- tibble(fid = fids, nos = 1)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "n", names(data))

              } else if(sourceClass %in% c("MULTIPOINT", "LINESTRING")){

                data <- x
                st_geometry(data) <- NULL
                fids <- unique(theCoords[, 3])
                nos <- unlist(lapply(fids, function(i){
                  length(which(theCoords[, 3] == i))
                }))
                new <- tibble(fids, nos)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "n", names(data))

              } else if(sourceClass %in% c("MULTILINESTRING", "POLYGON")){

                data <- x
                st_geometry(data) <- NULL
                fids <- unique(theCoords[, 4])
                nos <- unlist(lapply(fids, function(i){
                  length(which(theCoords[, 4] == i))
                }))
                new <- tibble(fids, nos)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "n", names(data))

              } else if(sourceClass %in% c("MULTIPOLYGON")){

                # check whether this is also required for the other types
                data <- x
                st_geometry(data) <- NULL
                dataName <- colnames(data)
                fids <- unique(theCoords[,5])
                nos <- lapply(fids, function(i){
                  temp <- theCoords[which(theCoords[,5] == i),]

                  lapply(unique(temp[,4]), function(j){
                    length(which(temp[,4] == j))
                  })
                })

                data <- data[rep(row.names(data), lengths(nos)),]
                data <- as_tibble(data)

                nos <- unlist(nos)
                fids <- seq_along(nos)

                new <- tibble(fids, nos)
                out <- bind_cols(new, data)
                colnames(out) <- c("fid", "n", dataName)
              }
            } else{
              # what happens if a sf-object has different feature-types?
            }

            return(out)
          })

#' @describeIn setTable set the attribute table of a \code{geom}
#' @importFrom dplyr left_join
#' @export

setMethod(f = "setTable",
          signature = "geom",
          definition = function(x, table){
            stopifnot(is.data.frame(table))
            stopifnot(any(names(table) %in% "fid"))
            nIDs <- length(x@attr$fid)
            x@attr <- left_join(x@attr, table)
            return(x)
          })

#' @describeIn getCoords get the table of coordinates of a \code{geom}
#' @importFrom tibble as_tibble
#' @examples
#' # coordinates of ...
#'
#' # ... a geom
#' getCoords(rtGeoms$mask)
#'
#' @export

setMethod(f = "getCoords",
          signature = "geom",
          definition = function(x){
            as_tibble(x@coords)
          })

#' @describeIn getCoords get the table of coordinates of any \code{Spatial*} object
#' @examples
#' # ... a Spatial* object
#' getCoords(rtSP$SpatialPoints)
#'
#' @importFrom tibble tibble as_tibble
#' @export

setMethod(f = "getCoords",
          signature = "Spatial",
          definition = function(x){

            theCoords <- NULL
            prev <- 0
            sourceClass <- class(x)[1]
            if(sourceClass %in% c("SpatialGrid")){
              x <- as(x, "SpatialPolygons")
            } else if(sourceClass %in% "SpatialGridDataFrame"){
              x <- as(x, "SpatialPolygonsDataFrame")
            } else if(sourceClass %in% "SpatialPixels"){
              x <- as(x, "SpatialPoints")
            } else if(sourceClass %in% "SpatialPixelsDataFrame"){
              x <- as(x, "SpatialPointsDataFrame")
            }
            sourceClass <- class(x)[1]

            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame")){

              theCoords <- bind_cols(fid = seq_along(x@coords[,1]),
                                     vid = seq_along(x@coords[,1]),
                                     as_tibble(x@coords))
              colnames(theCoords) <- c("vid", "fid", "x", "y")

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){

              temp <- x
              for(i in seq_along(temp@coords)){
                tempCoords <- tibble(fid = i,
                                     vid = seq_along(temp@coords[[i]][,1]),
                                     x = temp@coords[[i]][,1],
                                     y = temp@coords[[i]][,2])
                theCoords <- bind_rows(theCoords, tempCoords)
              }

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
                  polyCoords <- polyCoords[!duplicated(polyCoords),]

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
          })

#' @describeIn getCoords get the table of coordinates of any \code{sf} object
#' @importFrom tibble as_tibble
#' @importFrom sf st_geometry_type st_coordinates
#' @examples
#' # ... an sf* object
#' getCoords(rtSF$multilinestring)
#' @export

setMethod(f = "getCoords",
          signature = "sf",
          definition = function(x){

            sourceClass <- st_geometry_type(x)
            theCoords <- st_coordinates(x)
            # if(dim(theCoords)[1] > 1){
            #   theCoords <- theCoords[!duplicated(theCoords),]
            # }
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
                                    y = theCoords[,2],
                                    grp = theCoords[,3])

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
                                    y = theCoords[,2],
                                    grp = grps)

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
                                    y = theCoords[,2],
                                    grps = grps)

              }
            } else{
              # what happens if a sf-object has different feature-types?
            }

            return(theCoords)
          })

#' @describeIn getWindow get the reference window of a \code{geom}
#' @importFrom tibble as_tibble
#' @export

setMethod(f = "getWindow",
          signature = "geom",
          definition = function(x){
            as_tibble(x@window)
          })

#' @describeIn setWindow set the reference window of a \code{geom}
#' @export

setMethod(f = "setWindow",
          signature = "geom",
          definition = function(x, to){
            stopifnot(all(c("x", "y") %in% colnames(to)))
            if(nrow(to) == 4){
              x@window <- to[c("x", "y")]
            } else if(nrow(to) == 2){
              x@window <- data.frame(x = rep(to$x, each = 2),
                                     y = c(to$y, rev(to$y)))
            } else{
              stop("no suitable window provided.")
            }
            return(x)
          })

#' @describeIn getExtent get the bounding box of a \code{geom}
#' @importFrom dplyr bind_cols
#' @export

setMethod(f = "getExtent",
          signature = "geom",
          definition = function(x){
            bind_cols(x = c(min(x@coords$x), max(x@coords$x)),
                      y = c(min(x@coords$y), max(x@coords$y)))
          })

#' @describeIn getExtent get the bounding box of aany \code{Spatial*} object
#' @importFrom tibble tibble
#' @importFrom raster extent
#' @export

setMethod(f = "getExtent",
          signature = "Spatial",
          definition = function(x){
            ext <- extent(x)
            tibble(x = c(ext@xmin, ext@xmax),
                   y = c(ext@ymin, ext@ymax))
          })

#' @describeIn getExtent get the bounding box of a \code{sf} object
#' @importFrom tibble tibble
#' @importFrom sf st_bbox
#' @export

setMethod(f = "getExtent",
          signature = "sf",
          definition = function(x){
            ext <- st_bbox(x)
            tibble(x = c(ext[[1]], ext[[3]]),
                   y = c(ext[[2]], ext[[4]]))
          })

#' @describeIn getSubset get a subset of the features or vertices of a \code{geom}
#' @examples
#' aGeom <- rtGeoms$locations
#' (someVertices <- getSubset(aGeom, coords = "fid == 3"))
#' @export

setMethod(f = "getSubset",
          signature = c("geom"),
          definition = function(x, attr, coords){
            if(!missing(attr)){
              if(is.logical(attr)){
                stopifnot(dim(x@attr)[1] == length(attr))
                matches <- attr
              } else if(is.numeric(attr)){
                matches <- attr
              } else if(is.character(attr)){
                matches <- eval(parse(text = attr), envir = x@attr)
              }
              x@attr <- x@attr[matches,]
              x@coords <- x@coords[x@coords$fid %in% x@attr$fid,]
            }
            if(!missing(coords)){
              if(is.logical(coords)){
                stopifnot(dim(x@coords)[1] == length(coords))
                matches <- coords
              } else if(is.numeric(coords)){
                matches <- coords
              } else if(is.character(coords)){
                matches <- eval(parse(text = coords), envir = x@coords)
              }
              x@coords <- x@coords[matches,]
              x@attr <- x@attr[x@attr$fid %in% x@coords$fid,]

              nVids <- sapply(unique(x@coords$fid), function(i){
                length(x@coords$vid[x@coords$fid == i])
              })
              x@attr$n <- nVids
            }
            return(x)
          })

#' @describeIn getSubset get a subset of the features of a \code{sf} object
#' @examples
#'
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' (aSfc <- getSubset(nc, attr = "NAME %in% c('Ashe', 'Surry')"))
#' @export

setMethod(f = "getSubset",
          signature = c("sf"),
          definition = function(x, attr){
            if(!missing(attr)){
              if(is.logical(attr)){
                stopifnot(dim(x)[1] == length(attr))
                matches <- attr
              } else if(is.numeric(attr)){
                matches <- attr
              } else if(is.character(attr)){
                matches <- eval(parse(text = attr), envir = x)
              }
              x <- x[matches,]
            }
            return(x)
          })

#' @describeIn getSubset get a subset of the features of a \code{sp} object
#' @export

setMethod(f = "getSubset",
          signature = c("Spatial"),
          definition = function(x, attr){
            # if(!missing(attr)){
            #   if(is.logical(attr)){
            #     stopifnot(dim(x)[1] == length(attr))
            #     matches <- attr
            #   } else if(is.numeric(attr)){
            #     matches <- attr
            #   } else if(is.character(attr)){
            #     matches <- eval(parse(text = attr), envir = x)
            #   }
            #   x <- x[matches,]
            # }
            return(x)
          })

#' @describeIn getCRS get the coordinate reference system of a \code{geom}
#' @export

setMethod(f = "getCRS",
          signature = "geom",
          definition = function(x){
            x@crs
          })

#' @describeIn getCRS get the coordinate reference system of any \code{Spatial*} object
#' @export

setMethod(f = "getCRS",
          signature = "Spatial",
          definition = function(x){
            as.character(x@proj4string)
          })

#' @describeIn getCRS get the coordinate reference system of a \code{sf} object
#' @importFrom sf st_crs
#' @export

setMethod(f = "getCRS",
          signature = "sf",
          definition = function(x){
            st_crs(x)$proj4string
          })

#' @describeIn setCRS set the coordinate reference system of a \code{geom}
#' @importFrom stringr str_split
#' @export

setMethod(f = "setCRS",
          signature = "geom",
          definition = function(x, crs){
            if(is.na(x@crs)){
              x@crs <- crs
            } else{
              theCoords <- x@coords[which(names(x@coords) %in% c("x", "y"))]
              if(!all(c("+proj=longlat", "+ellps=WGS84") %in% str_split(x@crs, " ")[[1]])){
                geographic <- rgdal::project(as.matrix(theCoords), proj = as.character(x@crs), inv = TRUE)
              } else{
                geographic <- as.matrix(theCoords)
              }
              if(crs != "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"){
                projected <- rgdal::project(geographic, proj = as.character(crs))
              } else{
                projected <- geographic
              }
              x@coords <- data.frame(projected, x@coords[which(!names(x@coords) %in% c("x", "y"))])
              x@crs <- crs
              x <- setWindow(x = x, to = getExtent(x))
            }
            return(x)
          })

#' @describeIn setCRS set the coordinate reference system of any \code{Spatial*} object
#' @importFrom raster crs
#' @importFrom sp spTransform
#' @export

setMethod(f = "setCRS",
          signature = "Spatial",
          definition = function(x, crs){
            if(is.na(x@proj4string)){
              x@proj4string <- crs(crs)
            } else{
              x <- spTransform(x, CRSobj = crs(crs))
            }
            return(x)
          })

#' @describeIn setCRS set the coordinate reference system of a \code{sf} object
#' @importFrom raster crs
#' @importFrom sp spTransform
#' @export

setMethod(f = "setCRS",
          signature = "sf",
          definition = function(x, crs){

          })

#' @describeIn getHistory get the history of a \code{geom}
#' @export

setMethod(f = "getHistory",
          signature = "geom",
          definition = function(x){
            x@history
          })