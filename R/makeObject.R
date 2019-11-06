#' Make the object to a plot
#' @param x the object from which to make the plot.
#' @param window [\code{data.frame(1)}] two opposing corners of a rectangle to
#'   which the plot is limited.
#' @param image [\code{logical(1)}]\cr whether or not \code{x} is an image
#' @param theme [\code{gtTheme(1)}]\cr the theme from which to take graphical
#'   parameters.
#' @param ... instead of providing a \code{gtTheme}, you can also determine
#'   specific graphic parameters (see \code{\link{gpar}}) separately; see
#'   \code{\link{setTheme}} for details.
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#' @importFrom checkmate testCharacter testNames
#' @importFrom methods is
#' @importFrom grDevices colorRampPalette rgb
#' @export

makeObject <- function(x, window = NULL, image = FALSE, theme = gtTheme, ...){

  window <- .testWindow(x = window)

  featureType <- getType(x = x)
  theWindow <- getWindow(x = x)
  theExtent <- getExtent(x = x)

  displayArgs <- exprs(...)

  if(featureType[1] == "vector"){
    out <- list("type" = NULL, "name" = NULL, "window" = NULL, "out" = NULL, "hasLegend" = NULL, "params" = NULL, "legend" = NULL)

    thePoints <- getPoints(x = x)
    theFeatures <- getFeatures(x = x)
    theGroups <- getGroups(x = x)

    if(!is.null(window)){
      x <- setWindow(x = x, to = window)
    }

    tempArgs <- displayArgs[names(displayArgs) %in% names(theme@vector)]
    if(length(tempArgs) == 0){
      tempArgs <- setNames(list(theme@vector$scale$to), theme@vector$scale$x)
    }

    # make a table of relevant features
    attr <- thePoints
    if(!is.null(theFeatures)){
      attr <- left_join(x = attr, y = theFeatures, by = "fid")
    }
    if(!is.null(theGroups)){
      attr <- left_join(x = attr, y = theGroups, by = "gid")
    }

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

    if(!class(x)[1] %in% "geom"){
      x <- gc_geom(input = x)
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

    out$type <- "vector"
    out$name <- "geom"
    out$window <- tibble(x = c(min(theWindow$x), max(theWindow$x)),
                         y = c(min(theWindow$y), max(theWindow$y)))

  } else if(featureType[1] == "raster"){
    out <- list("type" = NULL, "extent" = NULL, "window" = NULL, "rows" = NULL, "cols" = NULL, "hasLegend" = NULL, "params" = NULL, "legend" = NULL, "values" = NULL)

    # set the user-provided window, if given
    if(!is.null(window)){
      theWindow <- window
    }

    if(image){

      if(featureType[2] == "matrix"){
        if(testCharacter(x = x[1], pattern = "\\#(.{6,8})")){
          theColours <- as.vector(x)
        } else {
          stop("to visualise an image, please provide a matrix with hexadecimal colour values (e.g. '#000000')")
        }
      } else if(grepl(x = featureType[2], pattern = "Raster")){
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
          stop("to visualise an image, please provide an object with the 3 layers 'red', 'green' and 'blue.'")
        }
      }
      out$hasLegend <- FALSE

    } else {

      if(testCharacter(x = x[1], pattern = "\\#(.{6,8})")){
        stop("to visualise an object with hexadecimal colour values (e.g. '#000000'), use 'image = TRUE'")
      }

      attr <- getFeatures(x = x) # this function must give the same for all raster/gridded objects
      if(!is.null(attr)){
        out$hasLegend <- TRUE
      } else {
        out$hasLegend <- FALSE
      }
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

      if(featureType[2] == "matrix") {
        allColours <- colorRampPalette(colors = targetColours)(nrVals)
        breaksTemp <- c(allValues[1]-1, seq(allValues[1], allValues[[length(allValues)]], length.out = nrVals))
      } else if(grepl(x = featureType[2], pattern = "Raster")){
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
      }

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
    out$values <- theColours

    out$type <- "raster"
    out$name <- names(x)
    out$extent <- getExtent(x = x)
    out$window <- tibble(x = c(min(theWindow$x), max(theWindow$x)),
                         y = c(min(theWindow$y), max(theWindow$y)))
    out$rows <- dim(x)[1]
    out$cols <- dim(x)[2]

  }

  return(out)
}

