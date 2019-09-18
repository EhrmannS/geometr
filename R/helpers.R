#' Make the layout of a plot
#'
#' @param x the object from which to make the plot.
#' @param window [\code{data.frame(1)}] two oposing corners of a rectangle to
#'   which the plot is limited.
#' @param theme [\code{gtTheme(1)}]\cr the theme from which to take graphical
#'   parameters.
#' @param image [\code{logical(1)}]\cr whether or not the raster (brick)
#'   contains an image.
#' @param ... []
#' @importFrom raster getValues
#' @export

makeLayout <- function(x = NULL, window = NULL, theme = gtTheme, image = FALSE, ...){

  window <- .testWindow(x = window, ...)

  # capture display arguments
  displayArgs <- exprs(...)

  attr <- getTable(x = x)
  params <- theme@geom

  # select only displayArgs that are part of the valid parameters.
  displayArgs <- displayArgs[names(displayArgs) %in% names(params)]
  if("fillcol" %in% names(displayArgs)){
    toEval <- as.character(displayArgs[which(names(displayArgs) == "fillcol")])
  } else if("linecol" %in% names(displayArgs)){
    toEval <- as.character(displayArgs[which(names(displayArgs) == "linecol")][[1]])
  } else {
    toEval <- NULL
  }
  if(!image){
    if(!is.null(toEval)){
      if(toEval %in% names(attr)){
        toEval <- names(attr)[which(names(attr) %in% toEval)]
      } else {
        toEval <- "fid"
      }
      arg <- eval(parse(text = paste0(toEval)), envir = attr)
    } else {
      arg <- eval(parse(text = "fid"), envir = attr)
    }
  } else {
    arg <- 0
  }
  arg <- as.character(arg)

  if(!is.null(window)){
    plotWin <- window
  } else {
    plotWin <- getWindow(x = x)
  }

  if(min(plotWin$x) == max(plotWin$x)){
    plotWin$x[1] <- plotWin$x[1] - 1
    plotWin$x[2] <- plotWin$x[2] + 1
  }
  if(min(plotWin$y) == max(plotWin$y)){
    plotWin$y[1] <- plotWin$y[1] - 1
    plotWin$y[2] <- plotWin$y[2] + 1
  }

  maxPlotX <- max(plotWin$x)
  minPlotX <- min(plotWin$x)
  maxPlotY <- max(plotWin$y)
  minPlotY <- min(plotWin$y)

  xBins <- theme@xAxis$bins
  yBins <- theme@yAxis$bins

  ratio <- list(x = (maxPlotX - minPlotX)/(maxPlotY - minPlotY),
                y = (maxPlotY - minPlotY)/(maxPlotX - minPlotX))
  xBinSize <- (maxPlotX - minPlotX)/xBins
  yBinSize <- (maxPlotY - minPlotY)/yBins
  axisSteps <- list(x1 = seq(from = minPlotX,
                             to = maxPlotX,
                             by = (maxPlotX - minPlotX)/xBins),
                    x2 = seq(from = minPlotX + (xBinSize/2),
                             to = maxPlotX,
                             by = (maxPlotX - minPlotX)/xBins),
                    y1 = seq(from = minPlotY,
                             to = maxPlotY,
                             by = (maxPlotY - minPlotY)/yBins),
                    y2 = seq(from = minPlotY + (yBinSize/2),
                             to = maxPlotY,
                             by = (maxPlotY - minPlotY)/yBins))
  margin <- list(x = (maxPlotX-minPlotX)*theme@yAxis$margin,
                 y = (maxPlotY-minPlotY)*theme@xAxis$margin)

  if(!is.null(window)){
    tempExt <- getExtent(x = x)
    minExtX <- min(tempExt$x)
    maxExtX <- max(tempExt$x)
    minExtY <- min(tempExt$y)
    maxExtY <- max(tempExt$y)

    xFactor <- abs(maxExtX - minExtX)/abs(maxPlotX - minPlotX)
    yFactor <- abs(maxExtY - minExtY)/abs(maxPlotY - minPlotY)
    xWindowOffset <- minPlotX / abs(maxExtX - minExtX)
    yWindowOffset <- minPlotY / abs(maxExtY - minExtY)
  } else{
    xFactor <- yFactor <- 1
    xWindowOffset <- yWindowOffset <- 0
  }

  if(theme@title$plot){
    titleH <- unit(theme@title$fontsize+6, units = "points")
  } else{
    titleH <- unit(0, "points")
  }
  if(theme@legend$plot){
    legendW <- ceiling(convertX(unit(1, "strwidth", arg[which.max(nchar(arg))]) + unit(30, "points"), "points"))
  } else{
    legendW <- unit(0, "points")
  }
  if(theme@yAxis$plot){
    yAxisTitleW <- unit(theme@yAxis$label$fontsize+6, units = "points")
    yAxisTicksW <- ceiling(convertX(unit(1, "strwidth", as.character(max(round(axisSteps$y1, theme@yAxis$ticks$digits)))), "points"))
  } else{
    yAxisTitleW <- unit(0, "points")
    yAxisTicksW <- unit(0, "points")
  }
  if(theme@xAxis$plot){
    xAxisTitleH <- unit(theme@xAxis$label$fontsize+6, units = "points")
    xAxisTicksH <- unit(theme@xAxis$ticks$fontsize, units = "points")
  } else{
    xAxisTitleH <- unit(0, "points")
    xAxisTicksH <- unit(0, "points")
  }
  xOffset <- ((as.numeric(yAxisTicksW) + as.numeric(yAxisTitleW)) - as.numeric(legendW))/2
  yOffset <- ((as.numeric(xAxisTicksH) + as.numeric(xAxisTitleH)) - as.numeric(titleH))/2

  # determine dimensions for this plot
  gridH <- unit(1, "grobheight", "panelGrob") - xAxisTitleH - xAxisTicksH - titleH
  gridHr <- unit(1, "grobwidth", "panelGrob")*ratio$y - yAxisTitleW*ratio$y - yAxisTicksW*ratio$y - legendW*ratio$y
  gridW <- unit(1, "grobwidth", "panelGrob") - yAxisTitleW - yAxisTicksW - legendW
  gridWr <- unit(1, "grobheight", "panelGrob")*ratio$x - xAxisTitleH*ratio$x- xAxisTicksH*ratio$x - titleH*ratio$x

  out <- list(minPlotX = minPlotX, #
              maxPlotX = maxPlotX, #
              minPlotY = minPlotY, #
              maxPlotY = maxPlotY, #
              xMajGrid = axisSteps$x1, #
              xMinGrid = axisSteps$x2, #
              yMajGrid = axisSteps$y1, #
              yMinGrid = axisSteps$y2, #
              xMargin = margin$x, #
              yMargin = margin$y, #
              xOffset = xOffset, #
              yOffset = yOffset, #
              xFactor = xFactor,
              yFactor = yFactor,
              gridH = gridH, #
              gridHr = gridHr, #
              gridW = gridW, #
              gridWr = gridWr, #
              titleH = titleH, #
              yAxisTicksW = yAxisTicksW, #
              xAxisTitleH = xAxisTitleH, #
              xWindowOffset = xWindowOffset, #
              yWindowOffset = yWindowOffset #
  )

  return(out)
}

#' Convert degree to radians
#' @param degree [\code{numeric(1)}]\cr a degree value to convert to radians.
#' @importFrom checkmate assertNumeric
#' @export

.rad <- function(degree){

  assertNumeric(x = degree)

  (degree * pi)/180
}

#' Get the number of decimal places
#' @param x [\code{numeric(1)}]\ the number for which to derive decimal places.
#' @importFrom checkmate assertNumeric
#' @export

.getDecimals <- function(x) {
  # https://stackoverflow.com/a/5173906/4506642

  assertNumeric(x = x)

  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }

}

#' Update the window
#'
#' Set the extent of a window to smaller/larger values, if the vertices would be
#' beyond the window otherwise.
#' @param input [\code{data.frame(1)}]\cr a table of vertices for which a new window should be derived.
#' @param window [\code{data.frame(1)}]\cr the old window.
#' @return A new window that has the extent of \code{input}.
#' @importFrom checkmate assertNames assertDataFrame
#' @export

.updateWindow <- function(input = NULL, window = NULL){

  # check arguments
  names(input) <- tolower(names(input))
  assertNames(x = names(input), must.include = c("x", "y"))
  assertDataFrame(x = input, min.rows = 2)
  names(window) <- tolower(names(window))
  assertNames(x = names(window), must.include = c("x", "y"))
  if(dim(window)[1] >= 2){
    window = as_tibble(data.frame(x = c(min(window$x), max(window$x), max(window$x), min(window$x), min(window$x)),
                                  y = c(min(window$y), min(window$y), max(window$y), max(window$y), min(window$y))))
  }
  assertDataFrame(x = window, nrows = 5)

  if(min(input$x) < min(window$x)){
    window$x[which.min(window$x)] <- min(input$x)
  }
  if(max(input$x) > max(window$x)){
    window$x[which.max(window$x)] <- max(input$x)
  }
  if(min(input$y) < min(window$y)){
    window$y[which.min(window$y)] <- min(input$y)
  }
  if(max(input$y) > max(window$y)){
    window$y[which.max(window$y)] <- max(input$y)
  }
  return(window)
}

#' Update the vertices
#'
#' Set the verties in a table so that they are valid for a geom.
#' @param input [\code{data.frame(1)}]\cr a table of vertices which should be
#'   brought into the correct form.
#' @importFrom checkmate assertNames assertDataFrame
#' @importFrom dplyr bind_cols group_by mutate distinct ungroup add_row bind_rows
#' @importFrom utils tail
#' @export

.updateVertices <- function(input = NULL){

  # check arguments
  names(input) <- tolower(names(input))
  assertNames(x = names(input), must.include = c("x", "y"), subset.of = c("x", "y", "fid"))
  assertDataFrame(x = input, min.rows = 2)

  # if no fid is specified, treat it as if all vertices are part of the same feature
  if(!"fid" %in% names(input)){
    input <- bind_cols(input, fid = rep(1, dim(input)[1]))
  }

  newRings <- oldRings <- NULL
  for(i in unique(input$fid)){
    temp <- input[input$fid == i,]
    temp$ring <- NA
    verts <- temp
    verts$seq <- seq_along(temp$fid)

    # determine the duplicated vertices that enclose other vertices per ring
    dups <- duplicated(temp, fromLast = TRUE) + duplicated(temp)
    if(any(dups > 0)){
      bounds <- verts[which(as.logical(dups)),]
      bounds <- group_by(.data = bounds, x, y, fid)
      bounds <- mutate(.data = bounds, min = min(seq), max = max(seq), seq = NULL)
      bounds <- distinct(.data = bounds)
      bounds <- ungroup(bounds)

      # label vertices
      lab <- el <- seq <- NULL
      full <- seq(from = min(bounds$min), to = max(bounds$max), by = 1)
      for(j in seq_along(bounds$fid)){
        lab <- c(lab, seq(from = bounds[j, ]$min, bounds[j, ]$max, by = 1))
        el <- c(el, rep(j, bounds[j, ]$max - bounds[j, ]$min + 1))
      }
      seq[lab] <- el
      temp$ring[lab] <- seq
      lastRing <- max(temp$ring, na.rm = TRUE)

    } else {
      temp$ring <- NA
      lastRing <- 0
    }

    # get values that are not yet part of a closed ring
    missingRing <- which(is.na(temp$ring))
    if(length(missingRing) > 0){
      oldRings <- bind_rows(oldRings, temp[-missingRing,])
    } else {
      oldRings <- bind_rows(oldRings, temp)
    }
    ind <- c(missingRing[-1], tail(missingRing, 1)+1) - missingRing

    # split up into list of separate rings
    splitBy <- NULL
    k <- 1
    for(j in seq_along(ind)){
      if(ind[j] > 1){
        splitBy <- c(splitBy, k)
        k <- k + 1
      } else {
        splitBy <- c(splitBy, k)
      }
    }
    missingRings <- split(missingRing, splitBy)

    # go through all rings and check them ...
    newRing <- NULL
    for(j in seq_along(missingRings)){
      aRing <- temp[missingRings[[j]], ]
      aRing$ring <- lastRing + 1

      dup <- duplicated(aRing, fromLast = TRUE)
      if(!dup[1]){
        aRing <- add_row(aRing, x = aRing$x[1], y = aRing$y[1], fid = aRing$fid[1], ring = lastRing + 1)
      }

      newRing <- bind_rows(newRing, aRing)
    }
    newRings <- bind_rows(newRings, newRing)
  }

  out <- bind_rows(oldRings, newRings)

  # go through each ring other than ring 1 and check whether they are inside
  # ring one
  parent <- out[out$ring == 1,]
  for(i in unique(out$ring)){
    if(i == 1){
      next
    }
    inside <- pointInGeomC(vert = as.matrix(out[out$ring == i,][c("x", "y")]),
                           geom = as.matrix(parent[c("x", "y")]),
                           invert = FALSE)

    if(any(inside != 1)){
      stop("some of the vertices are not within the outer ring.")
    }
  }
  out$ring <- NULL

  return(out)
}

#' Update column order
#'
#' Set the order of the table columns to \code{c("fid", "gid", rest)}
#' @param input [\code{data.frame(1)}]\cr a table othat contains at least the
#'   columns \code{fid} and \code{gid}.
#' @return A new table where the columns have the correct order.
#' @importFrom checkmate assertNames
#' @export

.updateOrder <- function(input = NULL){

  # check arguments
  targetCols <- c("fid", "gid")
  targetCols <- targetCols[targetCols %in% names(input)]

  out <- input[c(targetCols, names(input)[!names(input) %in% targetCols])]

  return(out)
}

#' Test anchor for consistency
#'
#' @param x [\code{data.frame | geom}]\cr the object to be tested for
#'   consistency.
#' @param ... [\code{.}]\cr additional arguments.
#' @importFrom checkmate testDataFrame testClass assertNames
#' @importFrom rlang exprs
#' @export

.testAnchor <- function(x, ...){

  out <- list()
  args <- exprs(..., .named = TRUE)

  if(testDataFrame(x = x, min.cols = 2)){
    out$type <- "df"
    colnames(x) <- tolower(colnames(x))
    assertNames(names(x), must.include = c("x", "y"), subset.of = c("x", "y", "fid"), .var.name = "anchor->names(x)")

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
#' @importFrom tibble as_tibble
#' @export

.testWindow <- function(x, ...){

  args <- exprs(..., .named = TRUE)

  if(testDataFrame(x = x, types = "numeric", any.missing = FALSE, ncols = 2)){
    colnames(x) <- tolower(colnames(x))
    assertNames(names(x), permutation.of = c("x", "y"), .var.name = "window->names(x)")
    if(dim(x)[1] >= 2){
      x = as_tibble(data.frame(x = c(min(x$x), max(x$x), max(x$x), min(x$x), min(x$x)),
                               y = c(min(x$y), min(x$y), max(x$y), max(x$y), min(x$y))))
      return(x)
    } else {
      return(NULL)
    }
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

#' Test colours
#'
#' @param colours [\code{.}]\cr the items to test for whether they are a colour
#'   value that is valid in R.
#' @details inspired by \href{https://stackoverflow.com/a/13290832}{https://stackoverflow.com/a/13290832}
#' @importFrom grDevices col2rgb
#' @export

.testColours <- function(colours = NULL) {
  sapply(colours, function(X) {
    tryCatch(is.matrix(col2rgb(X)), error = function(e) FALSE)
  })
}

#' Make a tiny map
#'
#' A tiny map is used via the show method of a geom.
#' @param geom [\code{geom}]\cr the geom from which to create a tiny map.
#' @export

.makeTinyMap <- function(geom = NULL){

  assertClass(x = geom, classes = "geom")
  theWindow <- getWindow(x = geom)
  featureType <- geom@type

  # get the window labels
  xmin <- round(min(geom@window$x), 2)
  xminFill <- paste0(rep(" ", nchar(xmin)), collapse = "")
  xmax <- round(max(geom@window$x), 2)
  xmaxFill <- paste0(rep(" ", nchar(xmax)), collapse = "")
  ymin <- round(min(geom@window$y), 2)
  ymax <- round(max(geom@window$y), 2)

  # define symbols
  full <- '\u25C9'
  half <- '\u25CE'
  quarter <- '\u25CB'
  empty <- '\u25CC'

  # create vector of symbols
  filled <- NULL
  nrPoints <- dim(geom@point)[1]
  for(i in 1:4){
    for(j in 1:4){
      x <- xmin + c(((xmax-xmin)/4 * j) - (xmax-xmin)/4, (xmax-xmin)/4 * j)
      y <- ymin + c(((ymax-ymin)/4 * i) - (ymax-ymin)/4, (ymax-ymin)/4 * i)
      target <- data.frame(x = c(x[1], x[2], x[2], x[1], x[1]),
                           y = c(y[1], y[1], y[2], y[2], y[1]))

      inside <- pointInGeomC(vert = as.matrix(geom@point[c("x", "y")]),
                             geom = as.matrix(target),
                             invert = FALSE)
      pointsInside <- sum(inside != 0)
      ratio <- pointsInside/nrPoints
      if(ratio < 1/16){
        recent <- empty
      } else if(ratio > 1/16 & ratio <= 1/8){
        recent <- quarter
      } else if(ratio > 1/8 & ratio <= 1/4){
        recent <- half
      } else if(ratio > 1/4){
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