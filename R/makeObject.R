#' Make the object to a plot
#' @param x the object from which to make object to plot.
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
             def = function(x, theme, ...){
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
          definition = function(x, theme, ...){

            out <- list()
            out$type <- "vector"
            out$name <- "geom"

            # test whether vertices are outside of 'window'
            inWindow <- pointInGeomC(vert = as.matrix(x@vert[c("x", "y")]),
                                     geom = as.matrix(x@window[c("x", "y")]),
                                     invert = FALSE)
            inWindow <- inWindow[-5] != 0

            if(!any(inWindow)){
              warning("no vertices are within the plotting window.", immediate. = TRUE)
            } else if(!all(inWindow)){
              warning("some vertices are not within the plotting window.", immediate. = TRUE)
            }

            # params <- exprs(...)
            # params <- params[names(params) %in% names(theme@geom)]

            aGrob <- gc_grob(input = x, theme = theme, ...)
            if(is(aGrob) != "gList"){
              aGrob <- gList(aGrob)
            }

            uniqueVals <- sapply(aGrob, function(x){
              x$name
            })
            uniqueColours <- sapply(aGrob, function(x){
              x$gp$col
            })
            uniqueValsNum <- seq_along(aGrob)

            # determine the tick values and labels
            if(length(uniqueValsNum) > theme@legend$bins){
              tickValues <- quantile(uniqueValsNum, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
            } else{
              tickValues <- uniqueValsNum
            }
            if(is.factor(uniqueVals) | is.character(uniqueVals)){
              labels <- uniqueVals[tickValues]
            } else {
              labels <- tickValues
            }

            if(theme@legend$ascending){
              colours <- tibble(colours = rev(uniqueColours),
                                values = rev(uniqueVals))
              legendPos <- tibble(labels = tickValues,
                                  pos = unit(tickValues, "native"))
            } else{
              colours <- tibble(colours = uniqueColours,
                                values = uniqueVals)
              legendPos <- tibble(labels = rev(tickValues),
                                  pos = rev(unit(tickValues, "native")))
            }

            out$out <- aGrob
            out$hasLegend <- TRUE
            out$uniqueValues <- colours
            out$legend <- legendPos

            return(out)
          }
)

#' @rdname makeObject
#' @param image [\code{logical(1)}]\cr whether or not the raster (stack)
#'   contains an image.
#' @importFrom checkmate testNames testCharacter assertClass
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "makeObject",
          signature = "Raster",
          definition = function(x, theme, image = FALSE, ...){

            out <- list()
            out$type <- "raster"
            out$name <- names(x)
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
              } else if(testCharacter(x = x[1], pattern = "\\#(.{6,8})")){
                theColours <- as.vector(x)
              } else{
                stop("please either provide rgb (in 3 layers) or hex values.")
              }
              out$hasLegend <- FALSE

            } else {
              assertClass(x = x, classes = "RasterLayer")
              out$hasLegend <- TRUE

              attr <- getTable(x)
              vals <- getValues(x)
              uniqueVals <- sortUniqueC(vals[!is.na(vals)])
              tickValues <- as.numeric(uniqueVals)
              nrVals <- length(uniqueVals)
              targetColours <- theme@raster$colours

              # limit values to 256, this is the number of distinct colours that
              # can be represented
              if(nrVals < 256){
                nrVals <- nrVals
              } else{
                nrVals <- 256
              }

              if(as.logical(length(x@legend@colortable))){
                uniqueColours <- x@legend@colortable[uniqueVals]
                breaksTemp <- c(uniqueVals[1]-1, uniqueVals)
              } else if(x@data@isfactor){
                uniqueColours <- colorRampPalette(colors = targetColours)(nrVals)
                idPos <- grep("id", colnames(attr), ignore.case = TRUE)
                breaksTemp <- c(uniqueVals[1]-1, attr[[idPos]])
              } else {
                uniqueColours <- colorRampPalette(colors = targetColours)(nrVals)
                breaksTemp <- c(uniqueVals[1]-1, seq(uniqueVals[1], uniqueVals[[length(uniqueVals)]], length.out = nrVals))
              }

              valCuts <- cut(vals, breaks = breaksTemp, include.lowest = TRUE)
              theColours <- uniqueColours[valCuts]
            }

            if(out$hasLegend){

              # determine the tick values and labels
              if(length(tickValues) > theme@legend$bins){
                tickValues <- quantile(tickValues, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
              }

              if(is.factor(vals) | is.character(vals)){
                tickLabels <- uniqueVals[tickValues]
              } else {
                tickLabels <- tickValues
              }

              if(theme@legend$ascending){
                colours <- tibble(colours = rev(uniqueColours),
                                  values = rev(uniqueVals))
                legendPos <- tibble(labels = tickLabels,
                                    pos = unit(tickValues, "native"))
              } else{
                colours <- tibble(colours = uniqueColours,
                                  values = uniqueVals)
                legendPos <- tibble(labels = rev(tickLabels),
                                    pos = rev(unit(tickValues, "native")))
              }
              out$uniqueValues <- colours
              out$legend <- legendPos
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
          definition = function(x, theme, image = FALSE, ...){

            out <- list()
            out$type <- "raster"
            out$name <- names(x)
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
              uniqueVals <- sortUniqueC(vals[!is.na(vals)])
              tickValues <- as.numeric(uniqueVals)
              nrVals <- length(uniqueVals)
              targetColours <- theme@raster$colours

              # limit values to 256, this is the number of distinct colours that
              # can be represented
              if(nrVals < 256){
                nrVals <- nrVals
              } else{
                nrVals <- 256
              }
              uniqueColours <- colorRampPalette(colors = targetColours)(nrVals)
              breaksTemp <- c(uniqueVals[1]-1, seq(uniqueVals[1], uniqueVals[[length(uniqueVals)]], length.out = nrVals))

              valCuts <- cut(vals, breaks = breaksTemp, include.lowest = TRUE)
              theColours <- uniqueColours[valCuts]

            }

            if(out$hasLegend){

              # determine the tick values and labels
              if(length(tickValues) > theme@legend$bins){
                tickValues <- quantile(tickValues, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
              }

              if(is.factor(vals) | is.character(vals)){
                tickLabels <- uniqueVals[tickValues]
              } else {
                tickLabels <- tickValues
              }

              if(theme@legend$ascending){
                colours <- tibble(colours = rev(uniqueColours),
                                  values = rev(uniqueVals))
                legendPos <- tibble(labels = tickLabels,
                                    pos = unit(tickValues, "native"))
              } else{
                colours <- tibble(colours = uniqueColours,
                                  values = uniqueVals)
                legendPos <- tibble(labels = rev(tickLabels),
                                    pos = rev(unit(tickValues, "native")))
              }
              out$uniqueValues <- colours
              out$legend <- legendPos
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
          definition = function(x, theme, ...){

            out <- list()
            out$type <- "vector"

            stop("plotting 'Spatial' objects is not yet supported.")
            return(out)
          }
)

#' @rdname makeObject
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "makeObject",
          signature = "sf",
          definition = function(x, theme, ...){

            out <- list()
            out$type <- "vector"

            stop("plotting 'sf' objects is not yet supported.")

            return(out)
          }
)