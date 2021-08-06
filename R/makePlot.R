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
#' @importFrom purrr map
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

  # if the parameter to scale has not beend defined as quick parameter, add it to 'plotParams'
  if(!theme@scale$param %in% names(plotParams) & !is.na(theme@scale$param)){
    plotParams <- c(plotParams, setNames(list(theme@scale$to), theme@scale$param))
  }

  # 2. update the theme ----
  # start_time <- Sys.time()
  if(featureType[1] == "grid"){
    plotParams <- list(fillcol = "gid")
    plotValues <- theFeatures$values
    if(is.numeric(plotValues)){
      scaleValues <- sortUniqueC(plotValues)
    } else {
      scaleValues <- sort(unique(plotValues))
    }
    scaleValues <- list(scaleValues)
  } else {
    plotValues <- map(.x = seq_along(plotParams), .f = function(ix){
      gt_pull(obj = x, var = plotParams[ix][[1]])
    })
    if(length(plotValues) == 0){
      plotValues <- theFeatures$gid
    }

    scaleValues <- map(.x = seq_along(plotValues), .f = function(ix){
      temp <- plotValues[[ix]]
      if(is.numeric(temp)){
        sortUniqueC(temp)
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

