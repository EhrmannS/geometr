#' Make the object to a plot
#' @param x the object from which to make the plot.
#' @param window [\code{data.frame(1)}] two opposing corners of a rectangle to
#'   which the plot is limited.
#' @param theme [\code{gtTheme(1)}]\cr the theme from which to take graphical
#'   parameters.
#' @name makeObject
#' @rdname makeObject
NULL

#' @rdname makeObject
#' @name makeObject
#' @export
if(!isGeneric("makeObject")){
  setGeneric(name = "makeObject",
             def = function(x, window, theme, ...){
               standardGeneric("makeObject")
             }
  )
}

#' @rdname makeObject
#' @param ... instead of providing a \code{gtTheme}, you can also determine
#'   specific graphic parameters (see \code{\link{gpar}}) separately; see
#'   \code{\link{setTheme}} for details.
#' @importFrom tibble as_tibble
#' @importFrom methods is
#' @export

setMethod(f = "makeObject",
          signature = "geom",
          definition = function(x, window = NULL, theme = gtTheme, ...){

            window <- .testWindow(x = window, ...)
            if(!is.null(window)){
              x <- setWindow(x = x, to = window)
            }

            displayArgs <- exprs(...)
            tempArgs <- displayArgs[names(displayArgs) %in% names(theme@vector)]
            if(length(tempArgs) == 0){
              tempArgs <- setNames(list(theme@vector$scale$to), theme@vector$scale$x)
            }

            featureType <- getType(x = x)[2]
            theWindow <- getWindow(x = x)
            theExtent <- getExtent(x = x)
            thePoints <- getPoints(x = x)
            theFeatures <- getTable(x = x, slot = "feature")
            theGroups <- getTable(x = x, slot = "group")

            if(featureType == "point"){
              attr <- left_join(x = thePoints, y = theFeatures, by = "fid")
              attr <- left_join(x = attr, y = theGroups, by = "gid")
            } else {
              attr <- left_join(x = theFeatures, y = theGroups, by = "gid")
            }

            out <- list()
            out$type <- "vector"
            out$name <- "geom"
            out$window <- tibble(x = c(min(theWindow$x), max(theWindow$x)),
                                 y = c(min(theWindow$y), max(theWindow$y)))

            # test whether vertices are outside of 'window'
            inWindow <- pointInGeomC(vert = as.matrix(thePoints[c("x", "y")]),
                                     geom = as.matrix(theWindow[c("x", "y")]),
                                     invert = FALSE)
            inWindow <- inWindow[-5] != 0

            if(!any(inWindow)){
              warning("no vertices are within the plotting window.", immediate. = TRUE)
            } else if(!all(inWindow)){
              warning("some vertices are not within the plotting window.", immediate. = TRUE)
            }

            aGrob <- gc_grob(input = x, theme = theme, ...)
            if(is(aGrob) != "gList"){
              aGrob <- gList(aGrob)
            }

            allValues <- sapply(aGrob, function(x){
              if(suppressWarnings(all(!is.na(as.numeric(as.character(x$name)))))){
                as.numeric(as.character(x$name))
              } else {
                x$name
              }
            })
            allColours <- sapply(aGrob, function(x){
              x$gp$col
            })
            allFill <- sapply(aGrob, function(x){
              if(!is.null(x$gp$fill)){
                x$gp$fill
              } else {
                NA_integer_
              }
            })
            allPch <- sapply(aGrob, function(x){
              if(!is.null(x$pch)){
                x$pch
              } else {
                NA_integer_
              }
            })
            allSize <- sapply(aGrob, function(x){
              if(!is.null(x$size)){
                x$size
              } else {
                NA_real_
              }
            })
            allLty <- sapply(aGrob, function(x){
              if(!is.null(x$gp$lty)){
                x$gp$lty
              } else {
                NA_character_
              }
            })
            allLwd <- sapply(aGrob, function(x){
              if(!is.null(x$gp$lwd)){
                x$gp$lwd
              } else {
                NA_integer_
              }
            })

            # make an overall table of parameters
            params <- tibble(fid = rev(allValues),
                             fillcol = rev(allFill),
                             pointsymbol = rev(allPch),
                             pointsize = rev(allSize),
                             linecol = rev(allColours),
                             linetype = rev(allLty),
                             linewidth = rev(allLwd))
            params <- left_join(x = params, y = attr, by = "fid")

            # go through the defined display arguments ...
            legends <- list()
            for(i in seq_along(tempArgs)){

              theArg <- names(tempArgs)[i]
              theVal <- as.character(tempArgs[[i]])

              # ... construct the indices for selecting attributes
              uniqueArg <- unique(eval(parse(text = theArg), envir = params))
              if(length(uniqueArg) > theme@legend$bins){
                tempTicks <- quantile(seq_along(uniqueArg), probs = seq(0, 1, length.out = theme@legend$bins), type = 1, names = FALSE)
              } else {
                tempTicks <- seq_along(uniqueArg)
              }

              # ... and select the attributes
              if(!theVal %in% names(params)){
                theVal <- "fid"
              }
              uniqueVal <- unique(eval(parse(text = theVal), envir = params))
              legendVals <- sort(uniqueVal)[tempTicks]

              tempLegend <- tibble(labels = legendVals,
                                   pos = as.numeric(unit(tempTicks, "native")))
              names(tempLegend)[1] <- as.character(theVal)

              legends <- c(legends, setNames(object = list(tempLegend), nm = theArg))
            }

            # revert order if given
            if(!theme@legend$ascending){
              params <- params[dim(params)[1]:1,]
              legendNames <- names(legends)
              legends <- lapply(seq_along(legends), function(x){
                legends[[x]][dim(legends[[x]])[1]:1,]
              })
              names(legends) <- legendNames
            }

            out$out <- aGrob
            out$hasLegend <- TRUE
            out$params <- params
            out$legend <- legends

            return(out)
          }
)

#' @rdname makeObject
#' @param image [\code{logical(1)}]\cr whether or not the raster (brick)
#'   contains an image.
#' @importFrom checkmate testNames testCharacter assertClass
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom raster getValues
#' @importFrom tibble as_tibble
#' @export

setMethod(f = "makeObject",
          signature = "Raster",
          definition = function(x, window = NULL, theme = gtTheme, image = FALSE, ...){

            out <- list()
            out$type <- "raster"
            out$name <- names(x)
            out$extent <- getExtent(x = x)
            if(!is.null(window)){
              theWindow <- window
            } else {
              theWindow <- getExtent(x = x)
            }
            out$window <- tibble(x = c(min(theWindow$x), max(theWindow$x)),
                                 y = c(min(theWindow$y), max(theWindow$y)))
            out$rows <- x@nrows
            out$cols <- x@ncols

            if(image){

              if(testNames(names(x), permutation.of = c("red", "green", "blue"))){
                alpha <- rep(255, length(x[[1]]))
                red <- getValues(x[[which(names(x) == "red")]])
                alpha[is.na(red)] <- 0L
                red[is.na(red)] <- 255L
                green <- getValues(x[[which(names(x) == "green")]])
                alpha[is.na(green)] <- 0L
                green[is.na(green)] <- 255L
                blue <- getValues(x[[which(names(x) == "blue")]])
                alpha[is.na(blue)] <- 0L
                blue[is.na(blue)] <- 255L
                theColours <- rgb(red = red, green = green, blue = blue, alpha = alpha, maxColorValue = 255)
              } else{
                stop("to visualise an image, please provide a RasterBrick with the 3 layers red, green, blue.")
              }
              out$hasLegend <- FALSE

            } else {
              assertClass(x = x, classes = "RasterLayer")
              out$hasLegend <- TRUE

              attr <- getTable(x = x)
              vals <- getValues(x = x)
              allValues <- sortUniqueC(vals[!is.na(vals)])
              tickValues <- seq_along(allValues)
              nrVals <- length(allValues)
              targetColours <- theme@raster$colours

              # limit values to 256, this is the number of distinct colours that
              # can be represented
              if(nrVals < 256){
                nrVals <- nrVals
              } else{
                nrVals <- 256
              }

              if(as.logical(length(x@legend@colortable))){
                allColours <- x@legend@colortable[allValues]
                breaksTemp <- c(allValues[1]-1, allValues)
              } else if(x@data@isfactor){
                allColours <- colorRampPalette(colors = targetColours)(nrVals)
                idPos <- grep("id", colnames(attr), ignore.case = TRUE)
                breaksTemp <- c(allValues[1]-1, attr[[idPos]])
              } else {
                allColours <- colorRampPalette(colors = targetColours)(nrVals)
                breaksTemp <- c(allValues[1]-1, seq(allValues[1], allValues[[length(allValues)]], length.out = nrVals))
              }

              valCuts <- cut(vals, breaks = breaksTemp, include.lowest = TRUE)
              theColours <- allColours[valCuts]
            }

            if(out$hasLegend){

              # determine the tick values and labels
              # rangetickValues <- c(tickValues, tail(tickValues, 1)+1)
              if(length(tickValues) > theme@legend$bins){
                tickValues <- quantile(tickValues, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
              }

              # if(is.factor(vals) | is.character(vals)){
              tickLabels <- allValues[tickValues]
              # } else {
              # tickLabels <- tickValues
              # }

              if(theme@legend$ascending){
                params <- tibble(fillcol = rev(allColours),
                                 values = rev(allValues))
                allValues <- rev(allValues)
                legendPos <- tibble(values = tickLabels,
                                    pos = unit(tickValues, "native"))
              } else{
                params <- tibble(fillcol = allColours,
                                 values = allValues)
                legendPos <- tibble(values = rev(tickLabels),
                                    pos = rev(unit(tickValues, "native")))
              }
              out$params <- params
              out$legend <- list(fillcol = legendPos)
            }
            out$array <- theColours

            return(out)
          }
)

#' @rdname makeObject
#' @importFrom tibble as_tibble
#' @importFrom grDevices colorRampPalette
#' @export

setMethod(f = "makeObject",
          signature = "matrix",
          definition = function(x, window = NULL, theme = gtTheme, image = FALSE, ...){

            out <- list()
            out$type <- "raster"
            out$name <- names(x)
            out$extent <- getExtent(x = x)
            if(!is.null(window)){
              theWindow <- window
            } else {
              theWindow <- getExtent(x = x)
            }
            out$window <- tibble(x = c(min(theWindow$x), max(theWindow$x)),
                                 y = c(min(theWindow$y), max(theWindow$y)))
            out$rows <- nrow(x)
            out$cols <- ncol(x)

            if(image){
              if(testCharacter(x = x[1], pattern = "\\#(.{6,8})")){
                theColours <- as.vector(x)
              } else {
                stop("to visualise an image, please provide a matrix with hexadecimal colour values (e.g. '#000000')")
              }
              out$hasLegend <- FALSE

            } else {
              out$hasLegend <- TRUE

              vals <- as.vector(t(x))
              assertNumeric(x = vals)
              allValues <- sortUniqueC(vals[!is.na(vals)])
              tickValues <- as.numeric(allValues)
              nrVals <- length(allValues)
              targetColours <- theme@raster$colours

              # limit values to 256, this is the number of distinct colours that
              # can be represented
              if(nrVals < 256){
                nrVals <- nrVals
              } else{
                nrVals <- 256
              }
              allColours <- colorRampPalette(colors = targetColours)(nrVals)
              breaksTemp <- c(allValues[1]-1, seq(allValues[1], allValues[[length(allValues)]], length.out = nrVals))

              valCuts <- cut(vals, breaks = breaksTemp, include.lowest = TRUE)
              theColours <- allColours[valCuts]

            }

            if(out$hasLegend){

              # determine the tick values and labels
              if(length(tickValues) > theme@legend$bins){
                tickValues <- quantile(tickValues, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
              }

              if(is.factor(vals) | is.character(vals)){
                tickLabels <- allValues[tickValues]
              } else {
                tickLabels <- tickValues
              }

              if(theme@legend$ascending){
                params <- tibble(fillcol = rev(allColours),
                                 values = rev(allValues))
                allValues <- rev(allValues)
                legendPos <- tibble(values = tickLabels,
                                    pos = unit(tickValues, "native"))
              } else{
                params <- tibble(fillcol = allColours,
                                 values = allValues)
                legendPos <- tibble(values = rev(tickLabels),
                                    pos = rev(unit(tickValues, "native")))
              }
              out$params <- params
              out$legend <- list(fillcol = legendPos)
            }
            out$array <- theColours

            return(out)
          }
)

#' @rdname makeObject
#' @importFrom tibble as_tibble
#' @export

setMethod(f = "makeObject",
          signature = "Spatial",
          definition = function(x, window = NULL, theme = gtTheme, ...){

            stop("visualising 'Spatial' objects is not yet supported.")
          }
)

#' @rdname makeObject
#' @importFrom tibble as_tibble
#' @export

setMethod(f = "makeObject",
          signature = "sf",
          definition = function(x, window = NULL, theme = gtTheme, ...){

            stop("visualising 'sf' objects is not yet supported.")
          }
)