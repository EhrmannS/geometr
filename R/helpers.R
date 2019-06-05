#' Gather colours for the plot
#'
#' @param input []
#' @param theme []
#' @param ... []
#' @importFrom stats setNames
#' @importFrom rlang rep_along
#' @export

.makeColours <- function(input = NULL, theme = NULL, ...){

  isRaster <- testClass(x = input, classes = "Raster")
  isGeom <- testClass(x = input, classes = "geom")

  attr <- getTable(input)
  params <- theme@geom

  if(isRaster){
    # get some meta of input
    hasColourTable <- as.logical(length(input@legend@colortable))
    isFactor <- input@data@isfactor
    vals <- getValues(input)
    uniqueVals <- sortUniqueC(vals[!is.na(vals)])
    uniqueValsNum <- as.numeric(uniqueVals)
    nrVals <- length(uniqueVals)
    targetColours <- theme@raster$colours

    # limit values to 256, this is the number of distinct colours that can be
    # represented
    if(nrVals < 256){
      nrVals <- nrVals
    } else{
      nrVals <- 256
    }

    if(hasColourTable){
      uniqueColours <- input@legend@colortable[uniqueVals]
      breaksTemp <- c(uniqueVals[1]-1, uniqueVals)
    } else if(isFactor){
      uniqueColours <- colorRampPalette(colors = targetColours)(nrVals)
      idPos <- grep("id", colnames(attr), ignore.case = TRUE)
      breaksTemp <- c(uniqueVals[1]-1, attr[[idPos]])
    } else {
      uniqueColours <- colorRampPalette(colors = targetColours)(nrVals)
      breaksTemp <- c(uniqueVals[1]-1, seq(uniqueVals[1], uniqueVals[[length(uniqueVals)]], length.out = nrVals))
    }

    valCuts <- cut(vals, breaks = breaksTemp, include.lowest = TRUE)
    out.cols <- uniqueColours[valCuts]

  } else if(isGeom){

    # get some meta of input
    hasColourTable <- FALSE
    isFactor <- FALSE
    out.cols <- NA_character_

    # capture display arguments
    displayArgs <- exprs(..., .named = TRUE)

    # when there are display arguments, take them, otherwise take the theme datault
    if(length(displayArgs) != 0){
      tempArgs <- displayArgs
    } else{
      tempArgs <- setNames(list(params$scale$to), params$scale$x)
    }
    if(!any(names(tempArgs) == "fillcol")){
      tempArgs <- c(tempArgs, setNames(list(NA_character_), "fillcol"))
    }

    if(all(c("linecol", "fillcol") %in% names(tempArgs))){
      if(all(is.na(c(tempArgs[[1]], tempArgs[[2]])))){
        stop("Either 'linecol' or 'fillcol' must contain a value other than 'NA'")
      }
    }

    defaultArgs <- params[!names(params) %in% names(tempArgs)]

    for(i in seq_along(tempArgs)){

      # determine value and name of the i-th display argument
      thisArg <- tempArgs[[i]]
      thisArgName <- names(tempArgs)[i]
      assertChoice(x = thisArgName, choices = names(params))
      pos <- which(names(params) %in% thisArgName)

      # check whether the parameter value is a column in 'attr', otherwise take
      # the default scale$to parameter
      if(!is.na(as.character(thisArg))){
        if(as.character(thisArg) %in% colnames(attr)){
          toEval <- thisArg
          toRamp <- params[[which(names(params) %in% thisArgName)]]
        } else{
          toEval <- as.symbol("fid")
          toRamp <- thisArg
        }

        vals <- eval(parse(text = paste0(toEval)), envir = attr)
        valsNum <- as.numeric(vals)
        uniqueVals <- unique(vals)
        uniqueValsNum <- as.numeric(uniqueVals)

        # if the argument is a colour argument, construct a color ramp from two or more values
        if(thisArgName %in% c("linecol", "fillcol")){
          params$scale$x <- thisArgName
          params$scale$cls <- thisArg

          uniqueColours <- colorRampPalette(colors = toRamp)(length(uniqueValsNum))
          breaks <- c(min(uniqueValsNum)-1, uniqueValsNum)
          valCuts <- cut(valsNum, breaks = breaks, include.lowest = FALSE)
          tempOut <- uniqueColours[valCuts]

        } else{
          tempOut <- rep_along(valsNum, thisArg)
        }

        params[[pos]] <- tempOut

      } else{
        params[[pos]] <- thisArg
      }
    }

    for(i in seq_along(defaultArgs)){
      if(i == 1) next

      thisArg <- defaultArgs[[i]][[1]]
      thisArgName <- names(defaultArgs)[i]
      pos <- which(names(params) %in% thisArgName)

      params[[pos]] <- rep(thisArg, dim(attr)[1])

    }

  }

  # determine the tick values and labels
  if(length(uniqueValsNum) > theme@legend$bins){
    tickValues <- quantile(uniqueValsNum, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
  } else{
    tickValues <- uniqueValsNum
  }
  if(is.factor(vals) | is.character(vals)){
    labels <- uniqueVals[tickValues]
  } else {
    labels <- tickValues
  }

  if(theme@legend$ascending){
    array <- matrix(data = rev(uniqueColours), ncol = 1, nrow = length(uniqueColours))
    values <- rev(uniqueVals)
    labelsPos <- unit(tickValues, "native")
  } else{
    array <- matrix(data = uniqueColours, ncol = 1, nrow = length(uniqueColours))
    values <- uniqueVals
    labelsPos <- rev(unit(tickValues, "native"))
  }

  out <- list(out.cols = out.cols,
              legend = list(
                array = array,
                values = values,
                labels = labels,
                labelsPos = labelsPos),
              params = params)
  return(out)
}


#' Gather the format of the plot
#'
#' @param panelExt []
#' @param theme []
#' @export

.makeFormat <- function(panelExt = NULL, theme = NULL){

  ratio <- list(x = (panelExt$x[2] - panelExt$x[1])/(panelExt$y[2] - panelExt$y[1]),
                y = (panelExt$y[2] - panelExt$y[1])/(panelExt$x[2] - panelExt$x[1]))
  xBins <- theme@xAxis$bins
  yBins <- theme@yAxis$bins
  xBinSize <- (panelExt$x[2] - panelExt$x[1])/xBins
  yBinSize <- (panelExt$y[2] - panelExt$y[1])/yBins
  axisSteps <- list(x1 = seq(from = panelExt$x[1],
                             to = panelExt$x[2],
                             by = (panelExt$x[2] - panelExt$x[1])/xBins),
                    x2 = seq(from = panelExt$x[1] + (xBinSize/2),
                             to = panelExt$x[2],
                             by = (panelExt$x[2] - panelExt$x[1])/xBins),
                    y1 = seq(from = panelExt$y[1],
                             to = panelExt$y[2],
                             by = (panelExt$y[2] - panelExt$y[1])/yBins),
                    y2 = seq(from = panelExt$y[1] + (yBinSize/2),
                             to = panelExt$y[2],
                             by = (panelExt$y[2] - panelExt$y[1])/yBins))
  margin <- list(x = (panelExt$x[2]-panelExt$x[1])*theme@yAxis$margin,
                 y = (panelExt$y[2]-panelExt$y[1])*theme@xAxis$margin)

  out <- list(xMajG = axisSteps$x1,
              xMinG = axisSteps$x2,
              yMajG = axisSteps$y1,
              yMinG = axisSteps$y2,
              xMar = margin$x,
              yMar = margin$y,
              xRat = ratio$x,
              yRat = ratio$y)
  return(out)
}

#' Convert degree to radians
#' @param degree [\code{numeric(1)}]\cr a degree value to convert to radians.
#' @importFrom checkmate assertNumeric
#' @export

.rad <- function(degree){
  assertNumeric(degree)
  (degree * pi)/180
}

#' Update the window slot
#'
#' Set the extent of a window to smaller/larger values, if the vertices would be
#' beyond the window otherwise.
#' @param geom [\code{geom}]\cr a geom for which a new window should be derived.
#' @param window [\code{tibble(1)}]\cr the old window.
#' @export

.updateWindow <- function(geom = NULL, window = NULL){
  if(min(geom$x) < min(window$x)){
    window$x[which.min(window$x)] <- min(geom$x)
  }
  if(max(geom$x) > max(window$x)){
    window$x[which.max(window$x)] <- max(geom$x)
  }
  if(min(geom$y) < min(window$y)){
    window$y[which.min(window$y)] <- min(geom$y)
  }
  if(max(geom$y) > max(window$y)){
    window$y[which.max(window$y)] <- max(geom$y)
  }
  return(window)
}

#' Test anchor for consistency
#'
#' @param x [\code{data.frame | geom}]\cr the object to be tested for
#'   consistency.
#' @param ... [\code{.}]\cr additional arguments.
#' @importFrom checkmate testDataFrame testClass assertNames
#' @importFrom dplyr bind_cols
#' @importFrom rlang exprs
#' @export

.testAnchor <- function(x, ...){

  out <- list()
  args <- exprs(..., .named = TRUE)

  if(testDataFrame(x = x, types = "numeric", any.missing = FALSE, min.cols = 2)){
    out$type <- "df"
  } else if(testClass(x = x, classes = "geom")){
    out$type <- "geom"
  } else {
    if("verbose" %in% names(args)){
      assertLogical(x = args$verbose)
      if(args$verbose){
        message("'anchor' is neither a data.frame nor a geom.")
      }
    }
    return(NULL)
  }

  if(out$type == "df"){
    colnames(x) <- tolower(colnames(x))
    assertNames(names(x), must.include = c("x", "y"), subset.of = c("x", "y", "fid"))
  }

  out$obj <- x

  return(out)
}

#' Test window for consistency
#'
#' @param x [\code{data.frame}]\cr the object to be tested for
#'   consistency.
#' @param ... [\code{.}]\cr additional arguments.
#' @importFrom checkmate testDataFrame assertNames
#' @importFrom rlang exprs
#' @export

.testWindow <- function(x, ...){

  args <- exprs(..., .named = TRUE)

  if(testDataFrame(x = x, types = "numeric", any.missing = FALSE, ncols = 2)){
    colnames(x) <- tolower(colnames(x))
    assertNames(names(x), must.include = c("x", "y"))
    return(x)
  } else {
    if("verbose" %in% names(args)){
      assertLogical(x = args$verbose)
      if(args$verbose){
        message("'window' is not a data.frame.")
      }
    }
    return(NULL)
  }

}

#' Test template for consistency
#'
#' @param x [\code{RasterLayer | matrix}]\cr the object to be tested for
#'   consistency.
#' @param ... [\code{.}]\cr additional arguments.
#' @importFrom checkmate testClass
#' @importFrom rlang exprs
#' @export

.testTemplate <- function(x, ...){

  out <- list()
  args <- exprs(..., .named = TRUE)

  if(testClass(x, "RasterLayer")){
    out$type <- "RasterLayer"
  } else if(testClass(x, "matrix")){
    out$type <- "matrix"
  } else {
    if("verbose" %in% names(args)){
      assertLogical(x = args$verbose)
      if(args$verbose){
        message("'template' is neither a RasterLayer nor a matrix.")
      }
    }
    return(NULL)
  }

  out$obj <- x

  return(out)
}

#' Make a tiny map
#'
#' A tiny map is used via the show method of a geom.
#' @param x [\code{geom}]\cr the geom from which to create a tiny map.
#' @importFrom cli symbol
#' @export

.makeTinyMap <- function(geom = NULL){

  assertClass(x = geom, classes = "geom")
  theWindow <- getWindow(x = geom)

  # get the window labels
  xmin <- round(min(geom@window$x), 2)
  xminFill <- paste0(rep(" ", nchar(xmin)), collapse = "")
  xmax <- round(max(geom@window$x), 2)
  xmaxFill <- paste0(rep(" ", nchar(xmax)), collapse = "")
  ymin <- round(min(geom@window$y), 2)
  ymax <- round(max(geom@window$y), 2)

  # define symbols
  full <- symbol$circle_filled
  half <- symbol$circle_double
  quarter <- symbol$circle
  empty <- symbol$circle_dotted

  # create vector of symbols
  filled <- NULL
  for(i in 1:4){
    for(j in 1:4){
      x <- xmin + c(((xmax-xmin)/4 * j) - (xmax-xmin)/4, (xmax-xmin)/4 * j)
      y <- ymin + c(((ymax-ymin)/4 * i) - (ymax-ymin)/4, (ymax-ymin)/4 * i)
      target <- data.frame(x = c(x[1], x[2], x[2], x[1], x[1]),
                           y = c(y[1], y[1], y[2], y[2], y[1]))

      # like so it makes sure that only the points are tested. not sure whether that is what I want. It's still problematic, becasue not all points are recognised as "inside" when they should be
      inside <- pointInGeomC(vert = as.matrix(geom@vert[c("x", "y")]),
                             geom = as.matrix(target),
                             invert = FALSE)
      inside <- sum(inside[-5])

      if(inside == 0){
        recent <- empty
      } else if(inside == 1){
        recent <- quarter
      } else if(inside == 2 | inside == 3){
        recent <- half
      } else {
        recent <- full
      }
      filled <- c(filled, recent)

    }
  }

  # populate the tiny map
  out <- paste0(c("", xminFill, ymax, "\n",
                  "          ", xminFill, filled[13], filled[14], filled[15], filled[16], xmaxFill, "\n",
                  "          ", xminFill, filled[9], filled[10], filled[11], filled[12], xmaxFill, "\n",
                  "          ", xminFill, filled[5], filled[6], filled[7], filled[8], xmaxFill, "\n",
                  "          ", xmin, filled[1], filled[2], filled[3], filled[4], xmax, "\n",
                  "          ", xminFill, ymin, ""))

  return(out)

}