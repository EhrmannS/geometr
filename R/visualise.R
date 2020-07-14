#' Visualise raster and geom objects
#'
#' @param ... objects to plot and optional graphical parameters.
#' @param layer [\code{integerish(.)} | \code{character(.)}]\cr in case the
#'   objects to plot have several layers, this is the name or index of the
#'   layer(s) that shall be plotted.
#' @param window [\code{data.frame(1)}]\cr two opposing corners of a rectangle
#'   to which the plot is limited.
#' @param theme [\code{list(7)}]\cr visualising options; see
#'   \code{\link{setTheme}} for details.
#' @param trace [\code{logical(1)}]\cr Print the raster object's history (i.e.
#'   the process according to which it has been created) (\code{TRUE}), or
#'   simply plot the object (\code{FALSE}, default).
#' @param image [\code{logical(1)}]\cr set this to \code{TRUE} if \code{raster}
#'   is actually an image; see Details.
#' @param new [\code{logical(1)}]\cr force a new plot (\code{TRUE}, default).
#' @param clip [\code{logical(1)}]\cr clip the plot by the plot box
#'   (\code{TRUE}, default), or plot all of the objects.
#' @details In case you want to plot an image (similar to
#'   \code{\link[raster]{plotRGB}}), you either have to: \enumerate{ \item
#'   provide a \code{RasterStack} with the three layers \code{red}, \code{green}
#'   and \code{blue} or \item provide a matrix with hexadecimal colour values
#'   (e.g. '#000000')} and set \code{image = TRUE}.
#'
#' @return Returns invisibly an object of class \code{recordedplot}, see
#'   \code{\link{recordPlot}} for details (and warnings).
#' @examples
#' coords <- data.frame(x = c(30, 60, 60, 40),
#'                      y = c(40, 40, 60, 70),
#'                      fid = 1)
#' (aGeom <- gs_polygon(anchor = coords))
#' visualise(aGeom)
#'
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' withWindow <- setWindow(x = aGeom, to = window)
#' visualise(expanded = withWindow)
#'
#' (aRaster <-  gtRasters$categorical)
#'
#' # plot several objects together
#' visualise(aRaster, aGeom)
#'
#' # give names
#' visualise(`a raster` = aRaster, `a geom` = aGeom)
#'
#' # use graphical parameters ...
#' visualise(aGeom, linecol = "green")
#'
#' # ... or a theme
#' visualise(aRaster, theme = setTheme(title = list(plot = FALSE)))
#'
#' @importFrom checkmate testClass testList assertNames assertList assertLogical
#'   testCharacter testIntegerish testNames
#' @importFrom rlang enquos eval_tidy
#' @importFrom tibble tibble
#' @importFrom grid grid.ls grid.newpage pushViewport viewport grid.rect
#'   grid.raster grid.clip unit grid.draw grid.grill upViewport grid.text gpar
#'   grid.get convertX downViewport grid.polyline grid.points unit.c
#' @importFrom grDevices recordPlot dev.list
#' @importFrom raster nlayers getValues as.matrix ncol nrow stack
#' @importFrom stats quantile
#' @export

visualise <- function(..., layer = NULL, window = NULL, theme = gtTheme, trace = FALSE, image = FALSE,
                      new = TRUE, clip = TRUE){

  # layer = NULL; window = NULL; theme = gtTheme; trace = FALSE; image = FALSE; new = TRUE; clip = TRUE; library(checkmate); library(grid); library(rlang)

  # check arguments ----
  window <- .testWindow(x = window)
  assertDataFrame(x = window, nrows = 5, min.cols = 2, null.ok = TRUE)
  assertClass(x = theme, classes = "gtTheme", null.ok = TRUE)
  assertLogical(x = trace, len = 1, any.missing = FALSE)
  assertLogical(x = image, len = 1, any.missing = FALSE)
  assertLogical(x = new, len = 1, any.missing = FALSE)
  assertLogical(x = clip, len = 1, any.missing = FALSE)

  # derive the objects to plot
  objs <- rlang::enquos(...)

  objects <- list()
  # get objects
  for(i in seq_along(objs)){
    argName <- names(objs)[i]

    if(!is.null(argName)){
      if(argName %in% names(theme@vector)){
        # exclude theme objects
        next
      }
    }
    theObject <- eval_tidy(expr = objs[[i]])
    if(!image){
      theObject <- getLayer(x = theObject, layer = layer)
      if(is.null(theObject)) stop(paste0("'", argName, "' is not an object that can be plotted with 'visualise()'."))
    } else {
      theObject <- list(theObject)
    }
    if(argName != ""){
      names(theObject) <- rep(argName, length(theObject))
    }
    objects <- c(objects, theObject)
  }

  # plot already open? ----
  if(!is.null(dev.list()) & !new){
    objViewports <- grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    newPlot <- ifelse(any(objViewports$name == "geometr"), FALSE, TRUE)
    panelNames <- objViewports$name[objViewports$vpDepth == 2 & objViewports$name != "1"]
    panelNames <- panelNames[!duplicated(panelNames)]
    panels <- length(panelNames)
  } else{
    newPlot <- TRUE
    panels <- length(objects)
  }
  objects <- rep(x = objects, length.out = panels)
  # return(objects)

  # checkup concerning plot size ----
  if(panels > 15){
    question <- readline(paste0("  -> this will produce ", panels, " panels, do you wish to continue? [yes/no]: "))
    question <- match.arg(question, c("yes", "no"))
    if(question == "no"){
      return(invisible(0))
    }
  }

  # determine the number of columns and rows and the position of panels
  if(panels > 1){
    ncol <- ceiling(sqrt(panels))
  } else{
    ncol <- 1
  }
  nrow <- ceiling(panels/ncol)
  panelPosY <- rep(rev(seq(from = 1, to = nrow)), each = ncol)
  panelPosX <- rep(seq(from = 1, to = ncol), times = nrow)

  if(newPlot){
    grid.newpage()
    pushViewport(viewport(name = "geometr"))
  }

  # plot the panels ----
  for(i in 1:panels){

    if(!newPlot){
      # set objects[[i]]@window to the previous panel extent
      prev <- grid.get(gPath("extentGrob"), global = TRUE)
      window <- .testWindow(x = tibble(x = c(as.numeric(prev$x), as.numeric(prev$x) + as.numeric(prev$width)),
                                       y = c(as.numeric(prev$y), as.numeric(prev$y) + as.numeric(prev$height))))
    }

    # make colours from   theme for the object ----
    obj <- makeObject(x = objects[i],
                      window = window,
                      image = image,
                      theme = theme,
                      ...)

    # make panel layout ----
    pnl <- makeLayout(x = obj, theme = theme)

    # ----
    if(newPlot | (!newPlot & obj$type == "raster")){

      # create the plot ----
      # open the panel viewport
      pushViewport(viewport(x = (panelPosX[i]/ncol)-(1/ncol/2),
                            y = (panelPosY[i]/nrow)-(1/nrow/2),
                            width = 1/ncol,
                            height = 1/nrow,
                            name = obj$name))
      grid.rect(width = convertX(unit(1, "npc"), "native"),
                gp = gpar(col = NA, fill = NA), name = "panelGrob")
      grid.rect(height = pnl$yMargin, width = pnl$xMargin,
                gp = gpar(fill = NA, col = NA), name = "marginGrob")
      grid.rect(x = unit(pnl$minPlotX, "points"), y = unit(pnl$minPlotY, "points"),
                height = unit(pnl$maxPlotY - pnl$minPlotY, "points"),
                width = unit(pnl$maxPlotX - pnl$minPlotX, "points"),
                gp = gpar(fill = NA, col = NA), name = "extentGrob")

      pushViewport(viewport(x = unit(0.5, "npc") + unit(pnl$xOffset, "points"),
                            y = unit(0.5, "npc") + unit(pnl$yOffset, "points"),
                            height = pnl$gridH,
                            width = pnl$gridW,
                            name = "plot"))

      # the title viewport
      if(theme@title$plot){
        pushViewport(viewport(name = "title"))
        grid.text(just = "top",
                  y = unit(1, "npc") - unit(3, "points") + pnl$titleH,
                  label = obj$name,
                  gp = gpar(fontsize = theme@title$fontsize,
                            col = theme@title$colour))
        upViewport() # exit title
      }

      # the yAxis viewport
      if(theme@yAxis$plot){
        pushViewport(viewport(xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                              yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                              name = "yAxis"))

        if(theme@yAxis$label$plot){
          grid.text(just = "right",
                    x = unit(0, "npc") - unit(2, "points") - pnl$yAxisTicksW,
                    label = theme@yAxis$label$title,
                    rot = theme@yAxis$label$rotation,
                    name = "title",
                    gp = gpar(fontsize = theme@yAxis$label$fontsize,
                              col = theme@yAxis$label$colour))
        }

        if(theme@yAxis$ticks$plot){
          grid.text(label = as.character(round(pnl$yMajGrid, theme@yAxis$ticks$digits)),
                    just = "right",
                    x = unit(-0.005, "npc"),
                    y = unit(pnl$yMajGrid, "native"),
                    rot = theme@xAxis$ticks$rotation,
                    name = "ticks",
                    gp = gpar(fontsize = theme@yAxis$ticks$fontsize,
                              col = theme@yAxis$ticks$colour))
        }
        upViewport() # exit yAxis
      }

      # the xAxis viewport
      if(theme@xAxis$plot){
        pushViewport(viewport(xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                              yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                              name = "xAxis"))

        if(theme@yAxis$label$plot){
          grid.text(just = "bottom",
                    y = unit(0, "npc") - unit(2, "points") - pnl$xAxisTitleH,
                    label = theme@xAxis$label$title,
                    rot = theme@xAxis$label$rotation,
                    name = "title",
                    gp = gpar(fontsize = theme@xAxis$label$fontsize,
                              col = theme@xAxis$label$colour))
        }

        if(theme@xAxis$ticks$plot){
          grid.text(label = as.character(round(pnl$xMajGrid, theme@xAxis$ticks$digits)),
                    just = "top",
                    x = unit(pnl$xMajGrid, "native"),
                    y = unit(-0.005, "npc"),
                    rot = theme@xAxis$ticks$rotation,
                    name = "ticks",
                    gp = gpar(fontsize = theme@xAxis$ticks$fontsize,
                              col = theme@xAxis$ticks$colour))
        }
        upViewport() # exit xAxis
      }

      # the legend viewport
      if(theme@legend$plot & obj$hasLegend){

        pushViewport(viewport(name = "legend"))

        # go through possible legend attributes and check whether it has more
        # than 1 unique value
        for(j in seq_along(obj$legend)){
          theParam <- names(obj$legend)[j]
          theLegend <- obj$legend[[j]]
          legendName <- names(theLegend[,1])

          if(length(theLegend$pos) == 1){
            maxYScale <- unit(as.numeric(theLegend$pos[length(theLegend$pos)]) + 1, "native")
          } else {
            maxYScale <- unit(as.numeric(theLegend$pos[which.max(theLegend$pos)]) + 1, "native")
          }
          pushViewport(viewport(height = unit(1, "npc") * theme@legend$sizeRatio,
                                yscale = c(1, maxYScale),
                                name = legendName))

          # this is a little hack to get all the values that are contained in the
          # object "into" the plotted object for later use (e.g. by gt_locate())
          theValues <- unlist(obj$params[legendName], use.names = FALSE)
          grid.text(label = theValues,
                    name = "legend_values",
                    gp = gpar(col = NA))


          if(theParam %in% c("linecol", "fillcol")){

            # 2. also make sure that the NA colour is always at the bottom (this
            # seems to be not the case when $range has a value)

            if(!is.null(theme@scale$bins)){
              theColours <- colorRampPalette(colors = theme@vector[[theParam]])(theme@scale$bins)
            } else {
              temp <- unlist(obj$params[legendName], use.names = FALSE)
              theColours <- unique(unlist(obj$params[order(temp),][theParam], use.names = FALSE))
            }
            grid.raster(x = unit(1, "npc") + pnl$legendX[j],
                        width = unit(10, "points"),
                        height = unit(1, "npc"),
                        just = "left",
                        name = "legend_items",
                        image = rev(theColours),
                        interpolate = FALSE)

            if(theme@legend$box$plot){
              grid.rect(x = unit(1, "npc") + pnl$legendX[j],
                        just = "left",
                        width = unit(10, "points"),
                        name = "legend_box",
                        gp = gpar(col = theme@legend$box$colour,
                                  fill = NA,
                                  lty = theme@legend$box$linetype,
                                  lwd = theme@legend$box$linewidth))
            }

          } else if(theParam %in% "pointsize"){

            theSizes <- sort(unique(unlist(obj$params[theParam], use.names = FALSE)))[theLegend$pos]
            grid.points(x = rep(unit(1, "npc") + pnl$legendX[j], times = length(theLegend$pos)),
                        y = unit(theLegend$pos, "native") - unit(0.5, "native"),
                        pch = theme@vector$pointsymbol[1],
                        size = unit(theSizes, "char"),
                        name = "legend_items")

          } else if(theParam %in% "pointsymbol"){

            theSymbols <- sort(unique(unlist(obj$params[theParam], use.names = FALSE)))[theLegend$pos]
            grid.points(x = rep(unit(1, "npc") + pnl$legendX[j], length(theSymbols)),
                        y = unit(theLegend$pos, "native") - unit(0.5, "native"),
                        pch = theSymbols,
                        size = unit(max(theme@vector$pointsize), "char"),
                        name = "legend_items")

          } else if(theParam %in% c("linewidth")){

            theWidths <- sort(unique(unlist(obj$params[theParam], use.names = FALSE)))[theLegend$pos]
            grid.polyline(x = rep(unit(c(1, 1), "npc") + unit.c(pnl$legendX[j], pnl$legendX[j] + unit(10, "points")), times = length(theLegend$pos)),
                          y = unit(rep(theLegend$pos, each = 2), "native") - unit(0.5, "native"),
                          id = rep(theLegend$pos, each = 2),
                          name = "legend_items",
                          gp = gpar(col = theme@vector$linecol[1],
                                    lwd = theWidths,
                                    lty = theme@vector$linetype[1]))

          } else if(theParam %in% c("linetype")){

            theTypes <- sort(unique(unlist(obj$params[theParam], use.names = FALSE)))[theLegend$pos]
            grid.polyline(x = rep(unit(c(1, 1), "npc") + unit.c(pnl$legendX[j], pnl$legendX[j] + unit(10, "points")), times = length(theLegend$pos)),
                          y = unit(rep(theLegend$pos, each = 2), "native") - unit(0.5, "native"),
                          id = rep(theLegend$pos, each = 2),
                          name = "legend_items",
                          gp = gpar(col = theme@vector$linecol[1],
                                    lwd = max(theme@vector$linewidth),
                                    lty = theTypes))

          }

          if(theme@legend$label$plot){
            grid.text(label = unlist(theLegend[legendName], use.names = FALSE),
                      x = unit(1, "npc") + pnl$legendX[j] + unit(15, "points"),
                      y = unit(theLegend$pos, "native") - unit(0.5, "native"),
                      name = "legend_labels",
                      just = c("left"),
                      gp = gpar(fontsize = theme@legend$label$fontsize,
                                col = theme@legend$label$colour))
          }

          # also include title for that legend

          upViewport() # exit current legend
        }

        upViewport() # exit legend
      }

      # the grid viewport
      pushViewport(viewport(xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                            yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                            name = "grid"))
      grid.rect(gp = gpar(col = NA, fill = NA), name = "gridGrob")

      # the box viewport
      if(theme@box$plot){
        pushViewport(viewport(width = unit(1, "npc") - unit(2*pnl$xMargin, "native"),
                              height = unit(1, "npc") - unit(2*pnl$yMargin, "native"),
                              name = "box"))
        grid.rect(gp = gpar(fill = theme@box$fillcol,
                            col = theme@box$linecol,
                            lwd = theme@box$linewidth,
                            lty = theme@box$linetype),
                  name = "theBox")
        upViewport() # exit box
      }

      # the grid viewport
      if(theme@grid$plot){

        # plot the major grid viewport
        pushViewport(viewport(xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                              yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                              name = "majorGrid"))

        grid.grill(h = unit(pnl$yMajGrid, "native"),
                   v = unit(pnl$xMajGrid, "native"),
                   gp = gpar(col = theme@grid$colour,
                             lwd = theme@grid$linewidth,
                             lty = theme@grid$linetype))
        upViewport() # exit majorGrid

        # plot the minor grid
        if(theme@grid$minor){
          pushViewport(viewport(xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                                yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                                name = "minorGrid"))

          grid.grill(h = unit(pnl$yMinGrid, "native"),
                     v = unit(pnl$xMinGrid, "native"),
                     gp = gpar(col = theme@grid$colour,
                               lwd = theme@grid$linewidth/2,
                               lty = theme@grid$linetype))
          upViewport() # exit minorGrid
        }
      }
      upViewport() # exit grid

      # the object viewport
      pushViewport(viewport(xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                            yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                            name = "object"))
      grid.rect(gp = gpar(col = NA, fill = NA), name = "objectGrob")

      if(obj$type == "raster"){
        pushViewport(viewport(width = unit(1, "npc") - unit(2 * pnl$xMargin, "native") + unit(theme@box$linewidth, "points"),
                              height = unit(1, "npc") - unit(2 * pnl$yMargin, "native") + unit(theme@box$linewidth, "points"),
                              xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                              yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                              name = "raster"))
        grid.clip(width = unit(1, "npc"),
                  height = unit(1, "npc"))
        grid.raster(x = unit(0, "npc") - unit(pnl$xWindowOffset, "npc") * pnl$xFactor,
                    y = unit(0, "npc") - unit(pnl$yWindowOffset, "npc") * pnl$yFactor,
                    width = unit(pnl$xFactor, "npc"),
                    height = unit(pnl$yFactor, "npc"),
                    hjust = 0,
                    vjust = 0,
                    image = matrix(data = obj$values, nrow = obj$rows, ncol = obj$cols, byrow = TRUE),
                    name = "theRaster",
                    interpolate = FALSE)
      } else if(obj$type == "vector") {
        pushViewport(viewport(width = unit(1, "npc") - unit(2 * pnl$xMargin, "native") + unit(theme@box$linewidth, "points"),
                              height = unit(1, "npc") - unit(2 * pnl$yMargin, "native") + unit(theme@box$linewidth, "points"),
                              xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                              yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                              name = "vector"))
        if(clip){
          grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "points"),
                    height = unit(1, "npc") + unit(theme@box$linewidth, "points"))
        }
        grid.draw(obj$out)
      }
      upViewport() # exit 'object'

      upViewport(3) # exit the object 'viewport' and 'plot' and 'obj$name'

    } else {

      downViewport(panelNames[i])
      downViewport("plot")
      pushViewport(viewport(xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                            yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                            name = "grid"))

      pushViewport(viewport(width = unit(1, "npc") - unit(2 * pnl$xMargin, "native"),
                            height = unit(1, "npc") - unit(2 * pnl$yMargin, "native"),
                            xscale = c(pnl$minPlotX - pnl$xMargin, pnl$maxPlotX + pnl$xMargin),
                            yscale = c(pnl$minPlotY - pnl$yMargin, pnl$maxPlotY + pnl$yMargin),
                            name = "geom"))

      if(clip){
        grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "points"),
                  height = unit(1, "npc") + unit(theme@box$linewidth, "points"))
      }
      grid.draw(obj$out)

      upViewport(4)
    }

    if(trace){

      theHist <- getHistory(x = objects[[i]])

      if(!is.null(theHist)){
        histMsg <- paste0("this object has the following history:\n -> ", paste0(theHist, collapse = "\n -> "))
        message(paste0(histMsg, collapse = "\n"))
      } else{
        message(paste0("this object has the following history:\n -> object loaded from memory"))
      }
    }

  }
  upViewport() # exit 'geometr'

  invisible(recordPlot(attach = "geometr"))
}

#' Create a new theme
#'
#' Assign parameters in a gtTheme to create a new theme.
#' @param from [\code{gtTheme}]\cr an gtTheme object.
#' @param title [\code{named list(.)}]\cr \code{plot = TRUE/FALSE},
#'   \code{fontsize} and \code{colour} of the title.
#' @param box [\code{named list(.)}]\cr \code{plot = TRUE/FALSE},
#'   \code{linewidth}, \code{linetype} and \code{linecol} of the bounding box
#'   (not supported recently).
#' @param xAxis [\code{named list(.)}]\cr \code{plot = TRUE/FALSE}, number of
#'   \code{bins} and \code{margin} of the x-axis,\cr\cr label [\code{named
#'   list(.)}]\cr \code{plot = TRUE/FALSE}, \code{title}, \code{fontsize},
#'   \code{colour} and \code{rotation} of the x-axis label,\cr\cr ticks
#'   [\code{named list(.)}]\cr \code{plot = TRUE/FALSE}, \code{fontsize},
#'   \code{colour} and number of \code{digits} to which to round the x-axis
#'   ticks.
#' @param yAxis [\code{named list(.)}]\cr \code{plot = TRUE/FALSE}, number of
#'   \code{bins} and \code{margin} of the y-axis,\cr\cr label [\code{named
#'   list(.)}]\cr \code{plot = TRUE/FALSE}, \code{title}, \code{fontsize},
#'   \code{colour} and \code{rotation} of the y-axis label,\cr\cr ticks
#'   [\code{named list(.)}]\cr \code{plot = TRUE/FALSE}, \code{fontsize},
#'   \code{colour} and number of \code{digits} to which to round the y-axis
#'   ticks.
#' @param grid [\code{named list(.)}]\cr \code{plot = TRUE/FALSE},
#'   \code{colour}, \code{linetype} and \code{linewidth} of the major and minor
#'   grid and whether or not to plot the \code{minor = TRUE/FALSE} grid.
#' @param legend [\code{named list(.)}]\cr \code{plot = TRUE/FALSE}, number of
#'   \code{bins}, \code{ascending = TRUE/FALSE} order of values and the
#'   \code{sizeRatio} of plot and legend, \cr\cr label [\code{named list(.)}]\cr
#'   \code{plot = TRUE/FALSE}, \code{fontsize} and \code{colour} of the legend
#'   labels, \cr\cr box [\code{named list(.)}]\cr \code{plot = TRUE/FALSE},
#'   \code{linetype}, \code{linewidth} and \code{colour} of the legend box.
#' @param scale [\code{named list(.)}]\cr \code{param = 'someParameter'} and
#'   \code{to = 'someAttribute'} to which to scale 'someParameter' to. Whether
#'   or not to use the values' \code{identity}, the value \code{range} that
#'   shall be represented by the scale and the number of \code{bins}.
#' @param vector [\code{named list(.)}]\cr \code{linecol}, \code{fillcol},
#'   \code{missingcol}, \code{linetype}, \code{linewidth}, \code{pointsize} and
#'   \code{pointsymbol} of a vector object.
#' @param raster [ \code{named list(.)}]\cr \code{fillcol} of a raster object.
#' @examples
#' input <- gtRasters$continuous
#' (myTheme <- setTheme(title = list(plot = FALSE)))
#'
#' visualise(input, theme = myTheme)
#' @importFrom checkmate assertList assertLogical assertNames
#' @export

setTheme <- function(from = NULL, title = NULL, box = NULL, xAxis = NULL,
                     yAxis = NULL, grid = NULL, legend = NULL, scale = NULL,
                     vector = NULL, raster = NULL){

  assertClass(x = from, classes = "gtTheme", null.ok = TRUE)
  if(is.null(from)){
    from <- gtTheme
  }
  out <- from

  assertList(title, any.missing = FALSE, max.len = 3, null.ok = TRUE)
  if(!is.null(title)){
    assertNames(names(title), subset.of = c("plot", "fontsize", "colour"))
    previous <- from@title
    for(i in seq_along(names(title))){
      out@title[which(names(previous) == names(title)[i])] <- title[i]
    }
  }

  assertList(box, any.missing = FALSE, max.len = 4, null.ok = TRUE)
  if(!is.null(box)){
    assertNames(names(box), subset.of = c("plot", "fillcol", "linewidth", "linetype", "linecol"))
    previous <- from@box
    for(i in seq_along(names(box))){
      out@box[which(names(previous) == names(box)[i])] <- box[i]
    }
  }

  assertList(xAxis, any.missing = FALSE, max.len = 5, null.ok = TRUE)
  if(!is.null(xAxis)){
    assertNames(names(xAxis), subset.of = c("plot", "bins", "margin", "label", "ticks"))
    if(any(names(xAxis) == "plot")){
      if(xAxis$plot == FALSE){
        xAxis$label$plot <- FALSE
        xAxis$ticks$plot <- FALSE
      }
    }
    for(i in seq_along(names(xAxis))){
      if(names(xAxis)[i] == "label"){
        assertList(xAxis$label, any.missing = FALSE, max.len = 5)
        assertNames(names(xAxis$label), subset.of = c("plot", "title", "fontsize", "colour", "rotation"))
        previous <- from@xAxis$label
        for(j in seq_along(names(xAxis$label))){
          out@xAxis$label[which(names(previous) == names(xAxis$label)[j])] <- xAxis$label[j]
        }
      }
      if(names(xAxis)[i] == "ticks"){
        assertList(xAxis$ticks, any.missing = FALSE, max.len = 5)
        assertNames(names(xAxis$ticks), subset.of = c("plot", "fontsize", "colour", "rotation", "digits"))
        previous <- from@xAxis$ticks
        for(i in seq_along(names(xAxis$ticks))){
          out@xAxis$ticks[which(names(previous) == names(xAxis$ticks)[i])] <- xAxis$ticks[i]
        }
      }
      if(!names(xAxis)[i] %in% c("label", "ticks")){
        previous <- from@xAxis[which(!names(from@xAxis) %in% c("label", "ticks"))]
        out@xAxis[which(names(previous) == names(xAxis)[i])] <- xAxis[i]
      }
    }
  }

  assertList(yAxis, any.missing = FALSE, max.len = 5, null.ok = TRUE)
  if(!is.null(yAxis)){
    assertNames(names(yAxis), subset.of = c("plot", "bins", "margin", "label", "ticks"))
    if(any(names(yAxis) == "plot")){
      if(yAxis$plot == FALSE){
        yAxis$label$plot <- FALSE
        yAxis$ticks$plot <- FALSE
      }
    }
    for(i in seq_along(names(yAxis))){
      if(names(yAxis)[i] == "label"){
        assertList(yAxis$label, any.missing = FALSE, max.len = 5)
        assertNames(names(yAxis$label), subset.of = c("plot", "title", "fontsize", "colour", "rotation"))
        previous <- from@yAxis$label
        for(i in seq_along(names(yAxis$label))){
          out@yAxis$label[which(names(previous) == names(yAxis$label)[i])] <- yAxis$label[i]
        }
      }
      if(names(yAxis)[i] == "ticks"){
        assertList(yAxis$ticks, any.missing = FALSE, max.len = 5)
        assertNames(names(yAxis$ticks), subset.of = c("plot", "fontsize", "colour", "rotation", "digits"))
        previous <- from@yAxis$ticks
        for(i in seq_along(names(yAxis$ticks))){
          out@yAxis$ticks[which(names(previous) == names(yAxis$ticks)[i])] <- yAxis$ticks[i]
        }
      }
      if(!names(yAxis)[i] %in% c("label", "ticks")){
        previous <- from@yAxis[which(!names(from@yAxis) %in% c("label", "ticks"))]
        out@yAxis[which(names(previous) == names(yAxis)[i])] <- yAxis[i]
      }
    }
  }

  assertList(grid, any.missing = FALSE, max.len = 5, null.ok = TRUE)
  if(!is.null(grid)){
    assertNames(names(grid), subset.of = c("plot", "minor", "colour", "linetype", "linewidth"))
    previous <- from@grid
    for(i in seq_along(names(grid))){
      out@grid[which(names(previous) == names(grid)[i])] <- grid[i]
    }
  }

  assertList(legend, any.missing = FALSE, max.len = 10, null.ok = TRUE)
  if(!is.null(legend)){
    assertNames(names(legend), subset.of = c("plot", "common", "bins", "ascending", "position", "sizeRatio", "title", "label", "ticks", "box"))

    for(i in seq_along(names(legend))){
      if(names(legend)[i] == "label"){
        assertList(legend$label, any.missing = FALSE, max.len = 3)
        assertNames(names(legend$label), subset.of = c("plot", "fontsize", "colour"))
        previous <- from@legend$label
        for(i in seq_along(names(legend$label))){
          out@legend$label[which(names(previous) == names(legend$label)[i])] <- legend$label[i]
        }
      }
      if(names(legend)[i] == "box"){
        assertList(legend$box, any.missing = FALSE, max.len = 4)
        assertNames(names(legend$box), subset.of = c("plot", "linetype", "linewidth", "colour"))
        previous <- from@legend$box
        for(i in seq_along(names(legend$box))){
          out@legend$box[which(names(previous) == names(legend$box)[i])] <- legend$box[i]
        }
      }
      if(!names(legend)[i] %in% c("title", "label", "ticks", "box")){
        previous <- from@legend[which(!names(from@legend) %in% c("title", "label", "ticks", "box"))]
        out@legend[which(names(previous) == names(legend)[i])] <- legend[i]
      }
    }
  }

  assertList(scale, any.missing = FALSE, max.len = 5, null.ok = TRUE)
  if(!is.null(scale)){
    assertNames(names(scale), subset.of = c("param", "to", "identity", "range", "bins"))
    previous <- from@scale
    for(i in seq_along(names(scale))){
      out@scale[which(names(previous) == names(scale)[i])] <- scale[i]
    }
  }

  assertList(vector, any.missing = FALSE, max.len = 7, null.ok = TRUE)
  if(!is.null(vector)){
    assertNames(names(vector), subset.of = c("linecol", "fillcol", "missingcol", "linetype", "linewidth", "pointsize", "pointsymbol"))
    previous <- from@vector
    for(i in seq_along(names(vector))){
      out@vector[which(names(previous) == names(vector)[i])] <- vector[i]
    }
  }

  assertList(raster, any.missing = FALSE, max.len = 2, null.ok = TRUE)
  if(!is.null(raster)){
    assertNames(names(raster), subset.of = c("fillcol"))
    previous <- from@raster
    for(i in seq_along(names(raster))){
      out@raster[which(names(previous) == names(raster)[i])] <- raster[i]
    }
  }

  return(out)
}
