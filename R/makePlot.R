#' Make the object to a plot
#' @param x [\code{list(1)}]\cr named list of the object from which to make the
#'   plot.
#' @param window [\code{data.frame(1)}] two opposing corners of a rectangle to
#'   which the plot is limited.
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

.makePlot <- function(x, window, theme = gtTheme, ...){

  # timings <- NULL

  out <- list(theme = NULL, grob = NULL, legend = NULL, layout = NULL)

  window <- .testWindow(x = window)

  # start_time <- Sys.time()
  featureType <- getType(x = x)
  thePoints <- getPoints(x = x)
  theFeatures <- getFeatures(x = x)
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "pull data", duration = end_time - start_time))

  # 1. manage plot parameters ----
  plotParams <- exprs(...)

  # only chose parameters that are in the theme (exclude plot objects)
  plotParams <- plotParams[names(plotParams) %in% c("linecol", "fillcol", "linetype", "linewidth", "pointsize", "pointsymbol")]


  # 2. update the theme ----
  # start_time <- Sys.time()
  if(featureType[1] == "grid"){
    theme@scale$param <- "fillcol"
    theme@scale$to <- "gid"
    plotValues <- theFeatures$values
    if(is.numeric(plotValues)){
      scaleValues <- sortUniqueC(plotValues)
    } else {
      scaleValues <- sort(unique(plotValues))
    }
  } else {
    if(length(plotParams) == 0){
      plotValues <- theFeatures$gid
    } else {
      plotValues <- gt_pull(obj = x, var = plotParams[1][[1]])
    }
    scaleValues <- sort(unique(plotValues))
  }

  if(dim(thePoints)[1] == 0){
    theme@title$plot <- FALSE
    theme@legend$plot <- FALSE
    theme@box$plot <- FALSE
  }

  # items <- suppressMessages(sort(gt_pull(obj = x, var = theme@scale$to)))
  if(!is.null(scaleValues)){
    if(length(scaleValues) > 10){
      testItems <- sample(scaleValues, 10)
    } else {
      testItems <- scaleValues
    }
    if(any(as.character(testItems) %in% colors()) | any(grepl(pattern = "\\#(.{6,8})", x = testItems))){
      theme@legend$plot <- FALSE
    }
    if(is.null(theme@scale$range)){
      if(!is.null(scaleValues)){
        theme@scale$range <- c(head(scaleValues, 1), tail(scaleValues, 1))
      }
    }
    if(is.null(theme@scale$bins)){
      if(!is.null(scaleValues)){
        theme@scale$bins <- length(scaleValues)
      }
    }
  }
  out$theme <- theme
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "update Theme", duration = end_time - start_time))

  # 3. make the grob ----
  # start_time <- Sys.time()
  rows <- ifelse(!is.null(getRows(x = x)), getRows(x = x), 0)
  cols <- ifelse(!is.null(getCols(x = x)), getCols(x = x), 0)
  theGrob <- .makeGrob(x = x,
                       featureType = featureType,
                       plotValues = plotValues,
                       scaleValues = scaleValues,
                       plotParams = plotParams,
                       rows = rows,
                       cols = cols,
                       theme = theme)
  out$grob <- theGrob
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "make grob", duration = end_time - start_time))

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
  theLayout <- .makeLayout(legend = theLegend,
                           window = window, #extent = extent,
                           theme = theme)
  out$layout <- theLayout
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "make layout", duration = end_time - start_time))

  return(out)
  # return(timings)
}

