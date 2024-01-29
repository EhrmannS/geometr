#' Make the layout of a plot
#'
#' @param x any spatial object to plot.
#' @param legend [list(.)][list]\cr the legend object built with
#'   \code{\link{.makeLegend}}.
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @param theme [geometr theme][gtTheme]\cr the theme from which to take
#'   graphical parameters.

.makeLayout <- function(x, legend, window, theme = gtTheme){

  maxWinX <- max(window$x)
  minWinX <- min(window$x)
  maxWinY <- max(window$y)
  minWinY <- min(window$y)

  objWindow <- getWindow(x = x)

  xZoom <- (max(objWindow$x) - min(objWindow$x)) / (maxWinX - minWinX)
  yZoom <- (max(objWindow$y) - min(objWindow$y)) / (maxWinY - minWinY)
  zoom <- min(c(xZoom, yZoom))

  xBins <- theme@xAxis$bins
  yBins <- theme@yAxis$bins

  ratio <- list(x = (maxWinX - minWinX)/(maxWinY - minWinY),
                y = (maxWinY - minWinY)/(maxWinX - minWinX))
  xBinSize <- (maxWinX - minWinX)/xBins
  yBinSize <- (maxWinY - minWinY)/yBins
  axisSteps <- list(x1 = seq(from = minWinX,
                             to = maxWinX,
                             by = (maxWinX - minWinX)/xBins),
                    x2 = seq(from = minWinX + (xBinSize/2),
                             to = maxWinX,
                             by = (maxWinX - minWinX)/xBins),
                    y1 = seq(from = minWinY,
                             to = maxWinY,
                             by = (maxWinY - minWinY)/yBins),
                    y2 = seq(from = minWinY + (yBinSize/2),
                             to = maxWinY,
                             by = (maxWinY - minWinY)/yBins))
  margin <- list(x = (maxWinX-minWinX)*theme@yAxis$margin,
                 y = (maxWinY-minWinY)*theme@xAxis$margin)

  if(theme@title$plot){
    titleH <- theme@title$fontsize+6
  } else{
    titleH <- 0
  }

  if(theme@legend$plot){

    legendH <- legendW <- NULL

    if(theme@legend$position == "right"){
      legendW <- 0
      legendScale <- NULL
      for(i in seq_along(legend)){
        labels <- legend[[i]]$children$legend_labels$label
        maxLbl <- labels[which.max(nchar(labels))]
        tempW <- as.numeric(ceiling(convertX(unit(1, "strwidth", maxLbl) + unit(25, "points"), "points")))
        legendW <- legendW + tempW
      }
      legendW <- unit(legendW, "points")
      legendH <- unit(0, "points")
    } else {
      legendH <- unit(0, "points")
      legendW <- unit(0, "points")
    }
  } else{
    legendW <- unit(0, "points")
    legendH <- unit(0, "points")
  }

  if(theme@legend$position == "right"){
    legendPosX <- 3
    legendPosY <- 2
  } else {
    legendPosX <- 2
    legendPosY <- 4
  }

  if(theme@yAxis$plot){
    yAxisTitleW <- theme@yAxis$label$fontsize + 5
    digits <- round(axisSteps$y1, theme@yAxis$ticks$digits)
    yAxisTicksW <- ceiling(convertX(unit(1, "strwidth", as.character(digits[which.max(nchar(digits))])), "points"))
    yAxisTicksW <- as.numeric(yAxisTicksW)
  } else{
    yAxisTitleW <- 0
    yAxisTicksW <- 0
  }
  if(theme@xAxis$plot){
    xAxisTitleH <- theme@xAxis$label$fontsize+2
    xAxisTicksH <- theme@xAxis$ticks$fontsize
  } else{
    xAxisTitleH <- 0
    xAxisTicksH <- 0
  }

  # determine dimensions for the plot
  gridH <- unit(1, "grobheight", "panelGrob") - unit(xAxisTitleH, "points") - unit(xAxisTicksH, "points") - unit(titleH, "points")
  gridW <- unit(1, "grobwidth", "panelGrob") - unit(yAxisTitleW, "points") - unit(yAxisTicksW, "points") - unit(legendW, "points")
  gridHr <- gridW*ratio$y
  gridWr <- gridH*ratio$x
  gridH <- min(gridH, gridHr) # keeping gridH and gridW as those "min" values allows the plot to change in size, when the plot window is changed in size
  gridW <- min(gridW, gridWr)

  out <- list(dim = list(x1 = yAxisTitleW+yAxisTicksW, x2 = gridW, x3 = legendW,
                         y1 = titleH, y2 = gridH, y3 = xAxisTitleH+xAxisTicksH, y4 = legendH), #y4 will be the legend height in case the position at the bottom is chosen
              zoom = zoom,
              margin = list(x = margin$x,
                            y = margin$y),
              window = list(xmin = minWinX,
                            xmax = maxWinX,
                            ymin = minWinY,
                            ymax = maxWinY),
              labels = list(titleH = titleH,
                            yAxisTicksW = yAxisTicksW,
                            xAxisTicksH = xAxisTicksH),
              scale = list(xmin = minWinX - margin$x,
                           xmax = maxWinX + margin$x,
                           ymin = minWinY - margin$y,
                           ymax = maxWinY + margin$y),
              grid = list(xMaj = axisSteps$x1,
                          xMin = axisSteps$x2,
                          yMaj = axisSteps$y1,
                          yMin = axisSteps$y2),
              legend = list(posX = legendPosX,
                            posy = legendPosY)
              )

  return(out)
}


#' Make the legend of a plot
#'
#' @param x any spatial object to plot.
#' @param scaleValues the scale values.
#' @param plotParams new plotting parameters specified
#'   via the quick options in \code{\link{visualise}}.
#' @param theme [geometr theme][gtTheme]\cr the theme from which to take
#'   graphical parameters.
#' @importFrom checkmate assertChoice
#' @importFrom grid textGrob rasterGrob rectGrob gpar gTree gList unit

.makeLegend <- function(x, scaleValues, plotParams, theme){

  # if(theme@legend$plot){

  legends <- list()
  prevX <- unit(0, "points")
  for(i in seq_along(plotParams)){

    allLabels <- scaleValues[[i]]
    theParam <- names(plotParams)[i]
    theVar <- plotParams[[i]]

    if(length(allLabels) > 10){
      testItems <- sample(allLabels, 10)
    } else {
      testItems <- allLabels
    }
    if(any(testItems %in% colors() | any(grepl(pattern = "\\#(.{6,8})", x = testItems)))){
      next
    }

    if(!is.null(theme@scale$bins)){
      thebins <- theme@scale$bins
    } else {
      thebins <- length(allLabels)
    }

    if(is.null(allLabels)){
      next
    }

    # determine the tick values and labels
    if(thebins > theme@legend$bins){
      tickPositions <- quantile(1:thebins, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
    } else {
      tickPositions <- 1:thebins
    }
    legendLabels <- allLabels[tickPositions]

    if(!theme@legend$ascending){
      tickPositions <- rev(tickPositions)
    }

    # this is a little hack to get all the values that are contained in the
    # object "into" the plotted object for later use (e.g. by geo_locate())
    legend_values <- textGrob(label = legendLabels,
                              name = "legend_values",
                              gp = gpar(col = NA))


    if(any(theParam == c("linecol", "fillcol"))){

      cols <- theme@parameters$colours
      allColours <- colorRampPalette(colors = cols)(thebins)

      legend_obj <- rasterGrob(x = unit(0, "npc") + unit(5, "points") + prevX,
                               width = unit(10, "points"),
                               height = unit(1, "npc"),
                               just = c("left"),
                               name = "legend_items",
                               image = rev(allColours),
                               interpolate = FALSE)

      if(theme@legend$box$plot){
        legend_obj <- gList(
          legend_obj,
          rectGrob(x = unit(0, "npc") + unit(5, "points") + prevX,
                   width = unit(10, "points"),
                   just = c("left"),
                   name = "legend_box",
                   gp = gpar(col = theme@legend$box$colour,
                             fill = NA,
                             lty = theme@legend$box$linetype,
                             lwd = theme@legend$box$linewidth)))
      }

    } else if(theParam == "pointsize"){

      theSizes <- seq(from = min(theme@parameters[["pointsize"]], na.rm = TRUE),
                      to = max(theme@parameters[["pointsize"]], na.rm = TRUE),
                      length.out = thebins)[tickPositions]

      legend_obj <- pointsGrob(x = rep(unit(0, "npc") + unit(10, "points") + prevX,
                                       times = length(tickPositions)),
                               y = unit(seq(0, 1, 1/(length(tickPositions)-1)), "npc"),
                               pch = 20,
                               size = unit(theSizes, "char"),
                               name = "legend_items")

    } else if(theParam == "linewidth"){

      theWidths <- seq(from = min(theme@parameters[["linewidth"]], na.rm = TRUE),
                       to = max(theme@parameters[["linewidth"]], na.rm = TRUE),
                       length.out = thebins)[tickPositions]

      legend_obj <- polylineGrob(x = rep(unit.c(unit(0, "points"), unit(10, "points")) + prevX,
                                         times = length(tickPositions)),
                                 y = unit(rep(seq(0, 1, 1/(length(tickPositions)-1)), each = length(tickPositions)), "npc"),
                                 id = rep(tickPositions, each = 2),
                                 name = "legend_items",
                                 gp = gpar(col = "black",
                                           lwd = theWidths,
                                           lty = "solid"))

    } else if(theParam == "pointsymbol"){

      theSymbols <- theme@parameters[["pointsymbol"]][tickPositions]

      legend_obj <- pointsGrob(x = rep(unit(0, "npc") + unit(10, "points") + prevX,
                                       times = length(tickPositions)),
                               y = unit(seq(0, 1, 1/(length(tickPositions)-1)), "npc"),
                               pch = theSymbols,
                               size = unit(0.5, "char"),
                               name = "legend_items")

    } else if(theParam %in% c("linetype")){

      theTypes <- theme@parameters[["linetype"]][tickPositions]

      legend_obj <- polylineGrob(x = rep(unit.c(unit(0, "points"), unit(10, "points")) + prevX,
                                         times = length(tickPositions)),
                                 y = unit(rep(seq(0, 1, 1/(length(tickPositions)-1)), each = length(tickPositions)), "npc"),
                                 id = rep(tickPositions, each = 2),
                                 name = "legend_items",
                                 gp = gpar(col = "black",
                                           lwd = 1,
                                           lty = theTypes))

    }

    if(theme@legend$label$plot){
      thePositions <- tickPositions / max(tickPositions)
      thePositions <- (tickPositions-1) / max(tickPositions) + thePositions[1]/2

      if(is.numeric(legendLabels)){
        legendLabels <- format(legendLabels, digits = theme@legend$digits+1)
      }

      legend_labels <- textGrob(label = legendLabels,
                                x = unit(0, "npc") + unit(20, "points") + prevX,
                                y = unit(thePositions, "npc"),
                                name = "legend_labels",
                                just = c("left", "centre"),
                                gp = gpar(fontsize = theme@legend$label$fontsize,
                                          col = theme@legend$label$colour))

      maxLbl <- legend_labels$label[which.max(nchar(legend_labels$label))]
      tempW <- as.numeric(ceiling(convertX(unit(1, "strwidth", maxLbl), "points")))
      prevX <- prevX + unit(20 + tempW, "points")
    } else {
      prevX <- prevX + unit(20, "points")
    }

    out <- gTree(children = gList(legend_values, legend_obj, legend_labels))
    legends <- c(legends, stats::setNames(list(out), theParam))
  }

  # } else {
  #   legends <- NULL
  # }

  return(legends)
}


#' Make the grob of a plot
#'
#' @param x the object to transform to class \code{grob}.
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @param layout layout to fit the grob into place.
#' @param featureType the type of feature to make a grob from.
#' @param plotValues the plot values.
#' @param scaleValues the scale values.
#' @param rows in case it's a grid, the number of rows.
#' @param cols in case it's a grid, the number of cols.
#' @param plotParams new plotting parameters specified
#'   via the quick options in \code{\link{visualise}}.
#' @param theme [geometr theme][gtTheme]\cr the theme from which to take
#'   graphical parameters.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#'   a \code{\link{polylineGrob}}, a \code{\link{pathGrob}} or a
#'   \code{\link{rasterGrob}}.
#' @importFrom rlang exprs rep_along
#' @importFrom geomio getPoints
#' @importFrom grDevices colorRampPalette colors rgb
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertNames assertSubset assertList
#' @importFrom dplyr left_join group_by mutate
#' @importFrom grid gpar unit pointsGrob gList pathGrob polylineGrob clipGrob
#'   rasterGrob

.makeGrob <- function(x, window, layout, featureType, plotValues, scaleValues,
                      plotParams, rows = rows, cols = cols, theme = gtTheme){

  if(theme@box$plot){

    if(featureType[1] != "grid") {

      params <- list(linecol = "black",
                     fillcol = NA,
                     linetype = "solid",
                     linewidth = 1,
                     pointsize = 0.5,
                     pointsymbol = 20)

      # process parameters that shall be changed
      for(i in seq_along(plotParams)){

        # determine value and name of the i-th display argument
        theVar <- plotParams[[i]]
        theParam <- names(plotParams)[i]
        pos <- which(names(params) %in% theParam)

        plotVals <- plotValues[[i]]
        scaleVals <- scaleValues[[i]]
        num <- suppressWarnings(as.numeric(as.character(theVar)))

        # if the argument is a colour argument, construct a color ramp from two or more values
        if(theParam %in% c("linecol", "fillcol")){

          if(!is.null(theme@scale$bins)){
            thebins <- theme@scale$bins
          } else {
            thebins <- length(scaleVals)
          }

          if(is.null(plotVals)){
            cols <- theVar
            if(!any(as.character(cols) %in% colors()) & !any(grepl(pattern = "\\#(.{6,8})", x = cols))){
              stop(paste0("'", cols, "' was neither found as column in the object to plot, nor is it a valid colour."))
            }
            tempOut <- colorRampPalette(colors = cols)(length(theVar))
          } else {
            cols <- theme@parameters$colours
            allColours <- colorRampPalette(colors = cols)(thebins)

            valCuts <- match(plotVals, sort(unique(plotVals)))
            tempOut <- allColours[valCuts]
          }

          if(!is.null(theme@parameters$missingcol)){
            tempOut[is.na(tempOut)] <- theme@parameters$missingcol
          }

        } else if(theParam %in% c("linewidth", "pointsize")){

          if(!is.null(theme@scale$bins)){
            thebins <- theme@scale$bins
          } else {
            thebins <- length(scaleVals)
          }

          if(is.null(plotVals)){
            if(is.na(num)){
              stop(paste0("'", theVar, "' was neither found as column in the object to plot, nor is it a valid ", theParam, "."))
            }
            tempOut <- num
          } else {
            allSizes <- seq(from = min(theme@parameters[[theParam]], na.rm = TRUE),
                            to = max(theme@parameters[[theParam]], na.rm = TRUE),
                            length.out = thebins)

            if(is.null(plotVals)){
              tempOut <- rep(num, length(allSizes))
            } else {
              valCuts <- match(plotVals, sort(unique(plotVals)))
              tempOut <- allSizes[valCuts]
            }
          }

        } else if(theParam %in% c("pointsymbol", "linetype")){

          if(!is.null(theme@scale$bins)){
            thebins <- theme@scale$bins
          } else {
            thebins <- length(scaleVals)
          }

          if(is.null(plotVals)){
            if(is.na(num)){
              stop(paste0("'", theVar, "' was neither found as column in the object to plot, nor is it a valid ", theParam, "."))
            }
            tempOut <- num
          } else {
            allSymbols <- theme@parameters[[theParam]]

            if(is.null(plotVals)){
              tempOut <- rep(num, length(allSymbols))
            } else {
              valCuts <- match(plotVals, sort(unique(plotVals)))
              tempOut <- allSymbols[valCuts]
            }
          }

        }

        params[[pos]] <- tempOut
      }

      # rescale values between 0 and 1
      x <- geo_scale(obj = x, range = tibble(x = c(0, 1), y = c(0, 1)))

      point <- getPoints(x = x)
      ids <- unique(point$fid)

      if(featureType[1] %in% "point"){

        out <- pointsGrob(x = unit(point$x, "npc"),
                          y = unit(point$y, "npc"),
                          pch = params$pointsymbol,
                          name = ids,
                          size = unit(params$pointsize, "char"),
                          gp = gpar(
                            col = params$linecol,
                            fill = params$fillcol))

      } else if(featureType[1] %in% "line"){

        out <- polylineGrob(x = unit(point$x, "npc"),
                            y = unit(point$y, "npc"),
                            id = as.numeric(as.factor(point$fid)),
                            name = ids,
                            gp = gpar(col = params$linecol,
                                      lty = params$linetype,
                                      lwd = params$linewidth))

      } else if(featureType[1] %in% "polygon"){

        dups <- group_by(.data = point, fid, x, y)
        dups <- mutate(.data = dups,
                       is_dup = duplicated(x) & duplicated(y),
                       is_odd = seq_along(fid) %% 2 == 0,
                       dup = as.integer(is_dup & is_odd))
        dups <- dups[["dup"]]
        dups <- c(0, dups[-length(dups)])
        vids <- 1 + cumsum(dups)

        out <- pathGrob(x = point$x,
                        y = point$y,
                        id = vids,
                        pathId = point$fid,
                        rule = "evenodd",
                        name = ids,
                        gp = gpar(
                          col = params$linecol,
                          fill = params$fillcol,
                          lty = params$linetype,
                          lwd = params$linewidth))
      }

    } else {

      scaleVals <- scaleValues[[1]]

      if(testCharacter(x = plotValues, pattern = "\\#(.{6,8})")){
        theColours <- as.vector(plotValues)
      } else {

        scaleBreaks <- c(scaleVals[1]-1, scaleVals)
        valCuts <- cut(plotValues, breaks = scaleBreaks, include.lowest = TRUE)

        colours <- theme@parameters$colours
        allColours <- colorRampPalette(colors = colours)(length(scaleVals))

        theColours <- allColours[valCuts]
      }

      out <- rasterGrob(x = -unit(layout$margin$x, "native"),
                        y = -unit(layout$margin$y, "native"),
                        width = unit(cols, "native") + unit(2 * layout$margin$x, "native") - unit(theme@box$linewidth, "points"),
                        height = unit(rows, "native") + unit(2 * layout$margin$y, "native") - unit(theme@box$linewidth, "points"),
                        hjust = 0,
                        vjust = 0,
                        image = matrix(data = theColours, nrow = rows, ncol = cols, byrow = TRUE),
                        name = "theRaster",
                        interpolate = FALSE)
    }

    if(is(out) != "gList"){
      out <- gList(out)
    }

  } else {
    out <- NULL
  }

  return(out)

}


#' Make the object to a plot
#' @param x [list(1)][list]\cr named list of the object from which to make the
#'   plot.
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @param theme [geometr theme][gtTheme]\cr the theme from which to take
#'   graphical parameters.
#' @param ... instead of providing a \code{gtTheme}, you can also determine
#'   specific graphic parameters (see \code{\link{gpar}}) separately; see
#'   \code{\link{setTheme}} for details.
#' @importFrom dplyr left_join
#' @importFrom geomio getType getPoints getFeatures getRows getCols sortUniqueCpp
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom checkmate testCharacter testNames
#' @importFrom methods is
#' @importFrom stats na.exclude
#' @importFrom grDevices colorRampPalette rgb

.makePlot <- function(x, window, theme = geoTheme, ...){

  # timings <- NULL

  out <- list(theme = NULL, grob = NULL, legend = NULL, layout = NULL)

  window <- .testWindow(x = window)

  # start_time <- Sys.time()
  featureType <- getType(x = x)
  thePoints <- getPoints(x = x)
  theFeatures <- getFeatures(x = x)
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "pull data", duration = end_time - start_time))

  # manage plot parameters ----
  plotParams <- exprs(...)

  # only chose parameters that are in the theme (exclude plot objects)
  plotParams <- plotParams[names(plotParams) %in% c("linecol", "fillcol", "linetype", "linewidth", "pointsize", "pointsymbol")]

  # if the parameter to scale has not beend defined as quick parameter, add it to 'plotParams'
  if(!theme@scale$param %in% names(plotParams) & !is.na(theme@scale$param)){
    plotParams <- c(plotParams, stats::setNames(list(theme@scale$to), theme@scale$param))
  }

  # update plot and scale values ----
  # start_time <- Sys.time()
  if(featureType[1] == "grid"){
    plotParams <- list(fillcol = "gid")
    plotValues <- theFeatures[[2]]
    if(is.numeric(plotValues)){
      scaleValues <- sortUniqueCpp(plotValues)
    } else {
      scaleValues <- sort(unique(plotValues))
    }
    scaleValues <- list(scaleValues)
  } else {
    plotValues <- map(.x = seq_along(plotParams), .f = function(ix){
      geo_pull(obj = x, var = plotParams[ix][[1]], ungroup = TRUE)
    })
    if(length(plotValues) == 0){
      plotValues <- theFeatures$gid
    }

    scaleValues <- map(.x = seq_along(plotValues), .f = function(ix){
      temp <- na.exclude(plotValues[[ix]])
      if(is.numeric(temp)){
        sortUniqueCpp(temp)
      } else {
        sort(unique(temp))
      }
    })
  }

  if(dim(thePoints)[1] == 0){
    theme@title$plot <- FALSE
    theme@legend$plot <- FALSE
    theme@box$plot <- FALSE
  }
  out$theme <- theme
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "update Theme", duration = end_time - start_time))
  # make the legend ----
  # start_time <- Sys.time()
  theLegend <- .makeLegend(x = x,
                           scaleValues = scaleValues,
                           plotParams = plotParams,
                           theme = theme)
  out$legend <- theLegend
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "make legend", duration = end_time - start_time))

  # make the layout ----
  # start_time <- Sys.time()
  theLayout <- .makeLayout(x = x,
                           legend = theLegend,
                           window = window, #extent = extent,
                           theme = theme)
  out$layout <- theLayout
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "make layout", duration = end_time - start_time))

  # make the grob ----
  # start_time <- Sys.time()
  rows <- ifelse(!is.null(getRows(x = x)), getRows(x = x), 0)
  cols <- ifelse(!is.null(getCols(x = x)), getCols(x = x), 0)
  theGrob <- .makeGrob(x = x,
                       window = window,
                       featureType = featureType,
                       plotValues = plotValues,
                       scaleValues = scaleValues,
                       plotParams = plotParams,
                       rows = rows,
                       cols = cols,
                       layout = theLayout,
                       theme = theme)
  out$grob <- theGrob
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "make grob", duration = end_time - start_time))


  return(out)
  # return(timings)
}


#' Make a tiny map
#'
#' A tiny map is used via the show method of a geom.
#' @param geom [gridded(1)][geom]\cr the geom from which to create a tiny map.
#' @importFrom checkmate assertClass
#' @importFrom geomio getWindow getPoints pointInPolyCpp

.makeTinyMap <- function(geom = NULL){

  assertClass(x = geom, classes = "geom")
  theWindow <- getWindow(x = geom)
  thePoints <- getPoints(x = geom)

  # get the window labels
  xmin <- round(min(theWindow$x), 2)
  xminFill <- paste0(rep(" ", nchar(xmin)), collapse = "")
  xmax <- round(max(theWindow$x), 2)
  xmaxFill <- paste0(rep(" ", nchar(xmax)), collapse = "")
  ymin <- round(min(theWindow$y), 2)
  ymax <- round(max(theWindow$y), 2)

  # define symbols
  full <- '\u25C9'
  half <- '\u25CE'
  quarter <- '\u25CB'
  empty <- '\u25CC'

  # create vector of symbols
  filled <- NULL
  nrPoints <- dim(thePoints)[1]
  for(i in 1:4){
    for(j in 1:4){
      x <- xmin + c(((xmax-xmin)/4 * j) - (xmax-xmin)/4, (xmax-xmin)/4 * j)
      y <- ymin + c(((ymax-ymin)/4 * i) - (ymax-ymin)/4, (ymax-ymin)/4 * i)
      target <- data.frame(x = c(x[1], x[2], x[2], x[1], x[1]),
                           y = c(y[1], y[1], y[2], y[2], y[1]))

      inside <- pointInPolyCpp(vert = as.matrix(thePoints[c("x", "y")]),
                               geom = as.matrix(target),
                               invert = FALSE)
      pointsInside <- sum(inside != 0)
      ratio <- pointsInside/nrPoints
      if(ratio <= 1/16){
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