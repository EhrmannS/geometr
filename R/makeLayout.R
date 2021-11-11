#' Make the layout of a plot
#'
#' @param x any spatial object to plot.
#' @param legend [\code{list(.)}]\cr the legend object built with
#'   \code{\link{.makeLegend}}.
#' @param window [\code{data.frame(1)}] two opposing corners of a rectangle to
#'   which the plot is limited.
#' @param theme [\code{gtTheme(1)}]\cr the theme from which to take graphical
#'   parameters.

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