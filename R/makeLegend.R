#' Make the legend of a plot
#'
#' @param x any spatial object to plot.
#' @param scaleValues the scale values.
#' @param plotParams new plotting parameters specified
#'   via the quick options in \code{\link{visualise}}.
#' @param theme the theme from which to take graphical
#'   parameters.
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
      # object "into" the plotted object for later use (e.g. by gt_locate())
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

