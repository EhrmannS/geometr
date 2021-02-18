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
#' @importFrom grid grid.ls grid.newpage pushViewport viewport grid.layout
#'   grid.rect grid.raster grid.clip unit grid.draw grid.grill upViewport
#'   grid.text gpar grid.get convertX downViewport grid.polyline grid.points
#'   unit.c
#' @importFrom grDevices recordPlot dev.list
#' @importFrom raster nlayers getValues as.matrix ncol nrow stack
#' @importFrom stats quantile
#' @export

visualise <- function(...,
                      layer = NULL,
                      window = NULL,
                      trace = FALSE,
                      image = FALSE,
                      new = TRUE,
                      clip = TRUE,
                      theme = gtTheme){

  # library(geometr); library(checkmate); library(grid); library(rlang); library(tibble); library(dplyr)
  # layer = NULL; window = NULL; theme = gtTheme; trace = FALSE; image = FALSE; new = T; clip = FALSE; newParams <- list()
  # source('/media/se87kuhe/external1/projekte/r-dev/geometr/R/makePlot.R')
  # source('/media/se87kuhe/external1/projekte/r-dev/geometr/R/updateTheme.R')
  # source('/media/se87kuhe/external1/projekte/r-dev/geometr/R/makeGrob.R')
  # source('/media/se87kuhe/external1/projekte/r-dev/geometr/R/makeLayout.R')
  # source('/media/se87kuhe/external1/projekte/r-dev/geometr/R/makeLegend.R')
  # source('/media/se87kuhe/external1/projekte/r-dev/geometr/R/test_functions.R')
  # Rcpp::sourceCpp('src/unique.cpp')

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
  if(any(!is.null(names(objs)))){
    objs <- objs[!names(objs) %in% names(theme@parameters)]
  }
  # if(length(objs) == 0){
  #   return()
  # }
  #
  # return(objs)

  # tease apart objects with several layers (e.g. RasterStack) and determine
  # names
  plotObjects <- plotNames <- NULL
  for(i in seq_along(objs)){
    theLayers <- getLayer(x = eval_tidy(expr = objs[[i]]))
    objsName <- names(objs)[i]
    layerName <- names(theLayers)

    if(objsName %in% names(theme@parameters)){
      next
    } else if(objsName == ""){
      theName <- layerName
      # theName <- getType(x = theLayers[[1]])[1]
    } else {
      theName <- rep(objsName, length(theLayers))
    }

    if(is.null(theLayers)){
      warning(paste0("object '", names(objs)[i], "' can't be plotted, it's neither a geometric object, nor a graphical parameter."))
    } else {
      plotObjects <- c(plotObjects, theLayers)
      plotNames <- c(plotNames, theName)
    }
  }
  names(plotObjects) <- plotNames
  # if(length(plotObjects) == 0){
  #   return()
  # }
  # return(plotObjects)

  # start_overall <- Sys.time()
  # timings <- NULL
  # start_time <- Sys.time()

  # plot already open? ----
  if(!is.null(dev.list()) & !new){
    objViewports <- grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    newPlot <- ifelse(any(objViewports$name == "geometr"), FALSE, TRUE)
    panelNames <- objViewports$name[objViewports$vpDepth == 2 & objViewports$name != "1"]
    panelNames <- panelNames[!duplicated(panelNames)]
    panels <- length(panelNames)
  } else{
    newPlot <- TRUE
    panels <- length(plotObjects)
  }
  plotObjects <- rep(x = plotObjects, length.out = panels)

  # checkup concerning plot size ----
  if(panels > 15){
    question <- readline(paste0("  -> this will produce ", panels, " panels, do you wish to continue? [yes/no]: "))
    question <- match.arg(question, c("yes", "no"))
    if(question == "no"){
      return(invisible(0))
    }
  }
  # end_time <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "prepare panel(s)", duration = end_time - start_time))

  # determine the number of columns and rows and the position of panels
  if(panels > 1){
    ncol <- ceiling(sqrt(panels))
  } else{
    ncol <- 1
  }
  nrow <- ceiling(panels/ncol)
  panelPosY <- rep(rev(seq(from = 1, to = nrow)), each = ncol)
  panelPosX <- rep(seq(from = 1, to = ncol), times = nrow)

  # start new plot ----
  if(newPlot){
    grid.newpage()
    pushViewport(viewport(name = "geometr"))
  }

  # return(plotObjects)

  # make and plot the panels ----
  for(i in seq_along(plotObjects)){

    # manage the object and its name ----
    theObject <- plotObjects[[i]]
    theType <- getType(x = plotObjects[[i]])[1]
    theName <- names(plotObjects)[i]

    if(newPlot | (!newPlot & theType == "grid")){

      # first, create the plot region for the i-th panel. This serve to derive all distances
      pushViewport(viewport(x = (panelPosX[i]/ncol)-(1/ncol/2),
                            y = (panelPosY[i]/nrow)-(1/nrow/2),
                            width = 1/ncol,
                            height = 1/nrow,
                            name = theName))
      grid.rect(width = convertX(unit(1, "npc"), "native"),
                gp = gpar(col = "green", fill = NA), name = "panelGrob")

      # prepare the object for plotting ----
      # start_time <- Sys.time()
      if(!is.null(window)){
        # if given, from the argument
        theWindow <- window
      } else {
        # and otherwise from the object to plot
        theWindow <- getWindow(x = theObject)
      }
      temp <- .makePlot(x = theObject, window = theWindow, image = image, theme = theme, ...)
      theGrob <- temp$grob
      theLegend <- temp$legend
      theLayout <- temp$layout
      # end_time <- Sys.time()
      # timings <- bind_rows(timings, tibble(activity = "make object", duration = end_time - start_time))


      # create the plot ----
      # grid.rect(height = theLayout$yMargin, width = theLayout$xMargin,
      #           gp = gpar(col = NA, fill = NA), name = "marginGrob")
      grid.rect(x = unit(theLayout$window$xmin, "points"),
                y = unit(theLayout$window$ymin, "points"),
                height = unit(theLayout$window$ymax - theLayout$window$ymin, "points"),
                width = unit(theLayout$window$xmax - theLayout$window$xmin, "points"),
                gp = gpar(col = NA, fill = NA), name = "extentGrob")

      # create the plot layout ----
      # start_time <- Sys.time()
      myLayout <- grid.layout(nrow = 4, ncol = 3,
                              widths = unit.c(unit(theLayout$dim$x1, "points"),
                                              theLayout$dim$x2,
                                              unit(theLayout$dim$x3, "points")),
                              heights = unit.c(unit(theLayout$dim$y1, "points"),
                                               theLayout$dim$y2,
                                               unit(theLayout$dim$y3, "points"),
                                               unit(theLayout$dim$y4, "points"))
      )
      # grid.show.layout(l = myLayout, newpage = FALSE)
      layoutVP <- viewport(name = "theLayout",
                           layout = myLayout)
      # 1. axis ticks and label dimensions need to be added
      # 2. raster must be smaller than the plot, by the margin
      titleVP <- viewport(name = "title",
                          layout.pos.col = 2, layout.pos.row = 1)
      yAxisVP <- viewport(name = "y_axis",
                          layout.pos.col = 1, layout.pos.row = 2,
                          xscale = c(theLayout$scale$xmin, theLayout$scale$xmax),
                          yscale = c(theLayout$scale$ymin, theLayout$scale$ymax))
      xAxisVP <- viewport(name = "x_axis",
                          layout.pos.col = 2, layout.pos.row = 3,
                          xscale = c(theLayout$scale$xmin, theLayout$scale$xmax),
                          yscale = c(theLayout$scale$ymin, theLayout$scale$ymax))
      legendVP <- viewport(name = "legend",
                           layout.pos.col = theLegend$posX, layout.pos.row = theLegend$posY)
      plotVP <- viewport(name = theType,
                         layout.pos.col = 2, layout.pos.row = 2,
                         xscale = c(theLayout$scale$xmin, theLayout$scale$xmax),
                         yscale = c(theLayout$scale$ymin, theLayout$scale$ymax))

      boxVP <- viewport(width = unit(1, "npc") - unit(2 * theLayout$margin$x, "native") + unit(theme@box$linewidth, "points"),
                        height = unit(1, "npc") - unit(2 * theLayout$margin$y, "native") + unit(theme@box$linewidth, "points"),
                        name = "box")
      legBoxVP <- viewport(height = unit(1, "npc") * theme@legend$yRatio,
                           width = unit(1, "npc") * theme@legend$xRatio,
                           yscale = c(1, theLayout$scale$legend),
                           name = "legend_box")

      # end_time <- Sys.time()
      # timings <- bind_rows(timings, tibble(activity = "create layout", duration = end_time - start_time))

      # do the plotting ... ----
      pushViewport(layoutVP)

      # ... the title viewport ----
      # start_time <- Sys.time()
      if(theme@title$plot){
        pushViewport(titleVP)
        grid.rect(gp = gpar(col = "blue", fill = NA))
        grid.text(y = unit(1, "npc") - unit(3, "points"),
                  just = "top",
                  label = theName,
                  gp = gpar(fontsize = theme@title$fontsize,
                            col = theme@title$colour))
        upViewport() # exit titleVP
      }

      # ... the yAxis viewport ----
      if(theme@yAxis$plot){
        pushViewport(yAxisVP)
        grid.rect(gp = gpar(col = "red", fill = NA))

        if(theme@yAxis$label$plot){
          grid.text(x = unit(1, "npc") - unit(3, "points") - unit(theLayout$labels$yAxisTicksW, "points"),
                    just = "right",
                    label = theme@yAxis$label$title,
                    rot = theme@yAxis$label$rotation,
                    name = "y_title",
                    gp = gpar(fontsize = theme@yAxis$label$fontsize,
                              col = theme@yAxis$label$colour))
        }

        if(theme@yAxis$ticks$plot){
          grid.text(x = unit(1, "npc") - unit(theLayout$labels$yAxisTicksW, "points"),
                    label = as.character(theLayout$grid$yMaj),
                    just = "left",
                    y = unit(theLayout$grid$yMaj, "native"),
                    rot = theme@xAxis$ticks$rotation,
                    name = "y_tick_labels",
                    gp = gpar(fontsize = theme@yAxis$ticks$fontsize,
                              col = theme@yAxis$ticks$colour))
        }
        upViewport() # exit yAxisVP
      }

      # ... the xAxis viewport ----
      if(theme@xAxis$plot){
        pushViewport(xAxisVP)
        grid.rect(gp = gpar(col = "red", fill = NA))

        if(theme@yAxis$label$plot){
          grid.text(y = unit(1, "npc") - unit(3, "points") - unit(theLayout$labels$xAxisTicksH, "points"),
                    just = "top",
                    label = theme@xAxis$label$title,
                    rot = theme@xAxis$label$rotation,
                    name = "x_title",
                    gp = gpar(fontsize = theme@xAxis$label$fontsize,
                              col = theme@xAxis$label$colour))
        }

        if(theme@xAxis$ticks$plot){
          grid.text(label = as.character(theLayout$grid$xMaj),
                    x = unit(theLayout$grid$xMaj, "native"),
                    y = unit(1, "npc") - unit(theLayout$labels$xAxisTicksH, "points"),
                    just = "bottom",
                    rot = theme@xAxis$ticks$rotation,
                    name = "x_tick_labels",
                    gp = gpar(fontsize = theme@xAxis$ticks$fontsize,
                              col = theme@xAxis$ticks$colour))
        }
        upViewport() # exit xAxisVP
      }
      # end_time <- Sys.time()
      # timings <- bind_rows(timings, tibble(activity = "plot margin", duration = end_time - start_time))

      # ... the legend viewport ----
      # start_time <- Sys.time()
      if(theme@legend$plot){
        pushViewport(legendVP)
        grid.rect(gp = gpar(col = "violet", fill = NA))
        pushViewport(legBoxVP)

        grid.draw(theLegend$obj)

        upViewport() # exit legBoxVP
        upViewport() # exit legendVP
      }
      # end_time <- Sys.time()
      # timings <- bind_rows(timings, tibble(activity = "plot legend", duration = end_time - start_time))

      # ... the plot viewport ----
      # start_time <- Sys.time()
      pushViewport(plotVP)

      if(theme@grid$plot){
        # plot the major grid viewport
        grid.grill(h = unit(theLayout$grid$yMaj, "native"),
                   v = unit(theLayout$grid$xMaj, "native"),
                   gp = gpar(col = theme@grid$colour,
                             lwd = theme@grid$linewidth,
                             lty = theme@grid$linetype))

        # plot the minor grid
        if(theme@grid$minor & theType != "grid"){
          grid.grill(h = unit(theLayout$grid$yMin, "native"),
                     v = unit(theLayout$grid$xMin, "native"),
                     gp = gpar(col = theme@grid$colour,
                               lwd = theme@grid$linewidth/2,
                               lty = theme@grid$linetype))
        }
      }

      if(theme@box$plot){
        pushViewport(boxVP)

        if(theType == "grid"){
          grid.clip(width = unit(1, "npc"),
                    height = unit(1, "npc"))
          grid.draw(theGrob)
        } else {
          if(clip){
            grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "points"),
                      height = unit(1, "npc") + unit(theme@box$linewidth, "points"))
          }
          grid.draw(theGrob)
        }

        upViewport() # exit boxVP
      }

      upViewport() # exit plotVP
      upViewport() # exit layoutVP
      upViewport() # exit 'theName'VP

      # end_time <- Sys.time()
      # timings <- bind_rows(timings, tibble(activity = "plot object", duration = end_time - start_time))

    } else {

      # prepare the object for plotting ----
      # start_time <- Sys.time()
      prev <- grid.get(gPath("extentGrob"), global = TRUE)
      if(is(prev) == "gList"){
        prev <- prev[[i]]
      }
      theWindow <- .testWindow(x = tibble(x = c(as.numeric(prev$x), as.numeric(prev$x) + as.numeric(prev$width)),
                                          y = c(as.numeric(prev$y), as.numeric(prev$y) + as.numeric(prev$height))))
      temp <- .makePlot(x = theObject, window = theWindow, image = image, theme = theme, ...)
      theGrob <- temp$grob
      # end_time <- Sys.time()
      # timings <- bind_rows(timings, tibble(activity = "make object", duration = end_time - start_time))

      downViewport(panelNames[i])
      downViewport("box")

      if(clip){
        grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "points"),
                  height = unit(1, "npc") + unit(theme@box$linewidth, "points"))
      }
      grid.draw(theGrob)
      upViewport()
      upViewport()
      upViewport()
      upViewport()
    }

    if(trace){

      theHist <- getHistory(x = theObject)

      if(!is.null(theHist)){
        histMsg <- paste0("this object has the following history:\n -> ", paste0(theHist, collapse = "\n -> "))
        message(paste0(histMsg, collapse = "\n"))
      } else{
        message(paste0("this object has the following history:\n -> object loaded from memory"))
      }
    }

  }
  upViewport() # exit 'geometr'


  # end_overall <- Sys.time()
  # timings <- bind_rows(timings, tibble(activity = "overall time", duration = end_overall - start_overall))

  invisible(recordPlot(attach = "geometr"))
  # return(timings)
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
#' @param parameters [\code{named list(.)}]\cr \code{linecol}, \code{fillcol},
#'   \code{missingcol}, \code{linetype}, \code{linewidth}, \code{pointsize} and
#'   \code{pointsymbol} of the plot object.
#' @examples
#' input <- gtRasters$continuous
#' (myTheme <- setTheme(title = list(plot = FALSE)))
#'
#' visualise(input, theme = myTheme)
#' @importFrom checkmate assertList assertLogical assertNames
#' @export

setTheme <- function(from = NULL, title = NULL, box = NULL, xAxis = NULL,
                     yAxis = NULL, grid = NULL, legend = NULL, scale = NULL,
                     parameters = NULL){

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

  assertList(parameters, any.missing = FALSE, max.len = 7, null.ok = TRUE)
  if(!is.null(parameters)){
    assertNames(names(parameters), subset.of = c("linecol", "fillcol", "missingcol", "linetype", "linewidth", "pointsize", "pointsymbol"))
    previous <- from@parameters
    for(i in seq_along(names(parameters))){
      out@parameters[which(names(previous) == names(parameters)[i])] <- parameters[i]
    }
  }

  return(out)
}
