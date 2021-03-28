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

  window <- .testWindow(x = window)

  plotParams <- exprs(...)

  # only chose parameters that are in the theme (exclude plot objects)
  plotParams <- plotParams[names(plotParams) %in% c("linecol", "fillcol", "linetype", "linewidth", "pointsize", "pointsymbol")]

  out <- list(theme = NULL, grob = NULL, legend = NULL, layout = NULL)

  # modify the theme according to 'plotParams' ----
  completeTheme <- .updateTheme(x = x, theme = theme)
  out$theme <- completeTheme

  # make the grob ----
  theGrob <- .makeGrob(x = x, plotParams = plotParams, theme = completeTheme)
  out$grob <- theGrob

  # make the legend ----
  theLegend <- .makeLegend(x = x, plotParams = plotParams, theme = completeTheme)
  out$legend <- theLegend

  # make the layout ----
  theLayout <- .makeLayout(legend = theLegend,
                           window = window, #extent = extent,
                           theme = completeTheme)
  out$layout <- theLayout

  return(out)
}

