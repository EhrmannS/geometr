#' Locate (and identify) clicks
#'
#' Click into a plot to get the location or identify values
#' @param samples [\code{integerish(1)}]\cr the number of clicks.
#' @param panel [\code{character(1)}]\cr the panel in which to locate (i.e. the
#'   title shown over the plot). Does not have to be accurate, as it matches
#'   with \code{\link{grep}}.
#' @param identify [\code{logical(1)}]\cr get the raster value at the sampled
#'   location (\code{TRUE}) or merely the location (\code{FALSE}, default).
#' @param snap [\code{logical(1)}]\cr should the returned value(s) be set to the
#'   nearest grid cell's center (\code{TRUE}) or should they remain the
#'   selected, "real" value (\code{FALSE}, default)?
#' @param raw [\code{logical(1)}]\cr should the complete statistics about the
#'   clicks be returned (\code{TRUE}), or should only the basic output be
#'   returned (\code{FALSE}, default)?
#' @param silent [\code{logical(1)}]\cr should all messages except errors be
#'   suppressed (\code{TRUE}), or should all messages be printed (\code{FALSE},
#'   default)?
#' @param show [\code{logical(1)}]\cr should information about the selected
#'   cells be included in the plot (\code{TRUE}), or should they merely be
#'   returned in the console (\code{FALSE, default})?
#' @param ... [\code{various}]\cr graphical parameters of the objects that are
#'   created when \code{show = TRUE}.
#' @return a \code{data.frame} of the selected locations and, if \code{identify
#'   = TRUE}, the respective values. If \code{show = TRUE} the values are also
#'   shown in the plot.
#' @examples
#' \dontrun{
#' # define a geometry
#' coords <- data.frame(x = c(30, 60, 60, 40),
#'                      y = c(40, 40, 60, 70),
#'                      fid = 1)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' (aGeom <- gs_polygon(anchor = coords, window = window, col = "blue"))
#'
#' # locate coordinates with geoms
#' visualise(geom = aGeom)
#' gt_locate(samples = 2)
#'
#' # locate or identify values with rasters
#' visualise(raster = gtRasters$continuous)
#' gt_locate(identify = TRUE, snap = TRUE)
#'
#' # with several panels, specify a target
#' visualise(raster = raster::stack(gtRasters$continuous, gtRasters$categorical))
#' gt_locate(samples = 4, panel = "categorical", snap = TRUE, identify = TRUE, show = TRUE)
#' }
#' @importFrom grDevices dev.list
#' @importFrom tibble tibble
#' @importFrom grid grid.ls grid.grep grid.force seekViewport grid.locator gList
#'   pointsGrob textGrob grid.draw upViewport unit grid.get gPath grid.points
#' @importFrom raster as.matrix
#' @export

gt_locate <- function(samples = 1, panel = NULL, identify = FALSE, snap = FALSE,
                      raw = FALSE, silent = FALSE, show = FALSE, ...){

  # check arguments
  assertIntegerish(x = samples, lower = 1, max.len = 1)
  assertCharacter(x = panel, ignore.case = TRUE, len = 1, null.ok = TRUE)
  assertLogical(x = identify, len = 1)
  assertLogical(x = snap, len = 1)
  assertLogical(x = silent, len = 1)
  assertLogical(x = show, len = 1)

  # test whether a geometr plot is already open
  if(!is.null(dev.list())){
    objViewports <- grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    mainVP <- grid.grep("vpLomm", grobs = FALSE, viewports = TRUE, grep = TRUE)
    if(!ifelse(any(mainVP == "vpLomm"), TRUE, FALSE)){
      stop("please create a plot with geometr::visualise()")
    }

    panelNames <- objViewports$name[objViewports$vpDepth == 2 & objViewports$name != "1"]
    panelNames <- panelNames[!duplicated(panelNames)]
  } else{
    stop("please create a plot with geometr::visualise()")
  }

  isLegendInPlot <- !identical(grid.grep("legend", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
  isRasterInPlot <- !identical(grid.grep("raster", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
  # if(isRasterInPlot & isLegendInPlot){
  #   canIdentify <- TRUE
  # } else{
  #   canIdentify <- FALSE
  # }
  isGeomInPlot <- !identical(grid.grep("geom", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
  # if(identify & !canIdentify){
  #   stop("to identify raster values the plot needs to contain a legend.")
  # }

  # get the panel in which locations should be determined
  if(is.null(panel)){
    if(length(panelNames) > 1){
      warning("no panel has been specified, so I chose the first panel. Please select locations there.", immediate. = TRUE, call. = FALSE)
    }
    panel <- panelNames[1]
  } else{
    panel <- panelNames[grepl(panel, panelNames)]
    if(length(panel) == 0){
      panel <- panelNames[1]
      warning("the specified panel did not match any of the existing panels, so I chose the first panel. Please select locations there.", immediate. = TRUE, call. = FALSE)
    }
  }

  # find the correct viewport to limit actions to this area of the plot
  if(isRasterInPlot){
    rasterVpPath <- grid.grep(paste0(panel, "::plot::grid::raster"), grobs = FALSE, viewports = TRUE, grep = TRUE)
    seekViewport(rasterVpPath)

    metaRaster <- grid.get(gPath("theRaster"), global = TRUE)
    if(length(panelNames) > 1){
      matCol <- as.matrix(metaRaster[which(panel == panelNames)][[1]]$raster)
    } else{
      matCol <- as.matrix(metaRaster$raster)
    }
  } else{
    raw <- FALSE
    snap <- FALSE
  }
  if(isGeomInPlot){
    geomVpPath <- grid.grep(paste0(panel, "::plot::grid::geom"), grobs = FALSE, viewports = TRUE, grep = TRUE)
    seekViewport(geomVpPath)
  }

  extentGrobMeta <- grid.get(gPath("extentGrob"))
  panelExt <- tibble(x = c(0, as.numeric(extentGrobMeta$width)) + as.numeric(extentGrobMeta$x),
                     y = c(0, as.numeric(extentGrobMeta$height)) + as.numeric(extentGrobMeta$y))

  if(identify){
    if(isLegendInPlot){
      metaLegend <- grid.get(gPath("theLegend"), global = TRUE)
      metaValues <- grid.get(gPath("legendValues"), global = TRUE)
      if(length(panelNames) > 1){
        legend <- metaLegend[which(panel == panelNames)][[1]]$raster
        values <- as.numeric(metaValues[which(panel == panelNames)][[1]]$label)
      } else{
        legend <- metaLegend$raster
        values <- as.numeric(metaValues$label)
      }
      matVal <- subChrIntC(matCol,
                           replace = legend,
                           with = values)
    } else{
      matVal <- matCol
    }
  }

  if(snap){
    theGrid <- data.frame(x = rep(seq(panelExt$x[1] + 0.5, panelExt$x[2], 1), times = panelExt$y[2]),
                          xmin = rep(seq(panelExt$x[1], panelExt$x[2]-1), times = panelExt$y[2]),
                          xmax = rep(seq(panelExt$x[1]+1, panelExt$x[2]), times = panelExt$y[2]),
                          y = rep(seq(panelExt$y[2]-0.5, panelExt$y[1]), each = panelExt$x[2]),
                          ymin = rep(seq(panelExt$y[2]-1, panelExt$y[1]), each = panelExt$x[2]),
                          ymax = rep(seq(panelExt$y[2], panelExt$y[1]+1), each = panelExt$x[2]))
  }

  if(!silent){
    message(paste0("please click ", samples, " location(s) in the panel '", panel, "'.\n\n"))
  }

  out <- NULL
  for(i in 1:samples){
    click <- grid.locator(unit = "npc")

    values <- round(as.numeric(click), 3)
    if(any(values < 0)) values <- c(NA, NA)

    # values need to be rescaled to the dimension of the marked window.
    values[1] <- ((panelExt$x[2] - panelExt$x[1])*(values[1] - 0) / (1 - 0)) + panelExt$x[1]
    values[2] <- ((panelExt$y[2] - panelExt$y[1])*(values[2] - 0) / (1 - 0)) + panelExt$y[1]

    # snap to the middle of the selected raster cells
    if(snap){
      matPos <- theGrid[which(values[1] > theGrid$xmin & values[1] <= theGrid$xmax &
                                values[2] > theGrid$ymin & values[2] <= theGrid$ymax),c(1, 4)]
      values[1] <- matPos$x
      values[2] <- matPos$y
      matPos <- data.frame(col = ceiling(matPos$x),
                           row = ceiling(matPos$y))
    } else{
      matPos <- data.frame(col = ceiling(values[1]),
                           row = ceiling(values[2]))
    }

    temp <- data.frame(id = i, x = values[[1]], y = values[[2]])

    if(raw){
      temp <- cbind(temp, matPos)
    }

    if(identify){
      theCol <- matCol[dim(matCol)[1]+1 - matPos$row, matPos$col]
      if(!is.null(matVal)){
        theVal <- matVal[dim(matCol)[1]+1 - matPos$row, matPos$col]
        plotVal <- theVal
      } else{
        theVal <- as.character(NA)
        plotVal <- theCol
      }
      vals <- data.frame(value = theVal,
                         colour = theCol)
      temp <- cbind(temp, vals)
    }

    if(show){
      if(identify){
        toDraw <- gList(pointsGrob(x = click$x,
                                   y = click$y,
                                   pch = 16,
                                   size = unit(1, "mm"),
                                   gp = gpar(...)),
                        textGrob(paste0("[", plotVal, "]"),
                                 click$x + unit(2, "mm"),
                                 click$y,
                                 just = "left",
                                 check.overlap = TRUE,
                                 gp = gpar(...))
        )
      } else{
        toDraw <- gList(pointsGrob(x = click$x,
                                   y = click$y,
                                   pch = 16,
                                   size = unit(1, "mm"),
                                   gp = gpar(...)),
                        textGrob(paste0("(", values[1], ", ", values[2], ")"),
                                 click$x + unit(2, "mm"),
                                 click$y,
                                 just = "left",
                                 check.overlap = TRUE,
                                 gp = gpar(...)))
      }
      grid.draw(toDraw)
    }
    out <- rbind(out, temp)

  }
  upViewport(5)

  return(out)
}


# from lattice::panel.identify (might be useful for identifying vertices to modify)
# function (x, y = NULL, subscripts = seq_along(x), labels = subscripts,
#           n = length(x), offset = 0.5, threshold = 18, panel.args = trellis.panelArgs(),
#           ...)
# {
#   if (missing(x)) {
#     x <- panel.args$x
#     y <- panel.args$y
#     if (missing(subscripts) && !is.null(panel.args$subscripts))
#       subscripts <- panel.args$subscripts
#   }
#   xy <- xy.coords(x, y, recycle = TRUE)
#   x <- xy$x
#   y <- xy$y
#   px <- convertX(unit(x, "native"), "points", TRUE)
#   py <- convertY(unit(y, "native"), "points", TRUE)
#   labels <- as.character(labels)
#   if (length(labels) > length(subscripts))
#     labels <- labels[subscripts]
#   unmarked <- rep(TRUE, length(x))
#   count <- 0
#   while (count < n) {
#     ll <- grid.locator(unit = "points")
#     if (is.null(ll))
#       break
#     lx <- convertX(ll$x, "points", TRUE)
#     ly <- convertY(ll$y, "points", TRUE)
#     pdists <- sqrt((px - lx)^2 + (py - ly)^2)
#     if (min(pdists, na.rm = TRUE) > threshold)
#       warning("no observations within ", threshold, " points")
#     else {
#       w <- which.min(pdists)
#       if (unmarked[w]) {
#         pos <- getTextPosition(x = lx - px[w], y = ly -
#                                  py[w])
#         ltext(x[w], y[w], labels[w], pos = pos, offset = offset,
#               ..., identifier = "identify")
#         unmarked[w] <- FALSE
#         count <- count + 1
#       }
#       else warning("nearest observation already identified")
#     }
#   }
#   subscripts[!unmarked]
# }
