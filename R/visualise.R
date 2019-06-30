#' Visualise raster and geom objects
#'
#' @param ... objects to plot and optional graphical parameters.
#' @param window [\code{data.frame(1)}]\cr two oposing corners of a rectangle to
#'   which the plot is limited.
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
#' @details In case you want to plot an image (simiar to
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
#' @importFrom grid grid.newpage pushViewport viewport grid.rect grid.raster
#'   grid.clip unit grid.draw grid.grill upViewport grid.text gpar convertX
#'   downViewport
#' @importFrom grDevices recordPlot
#' @importFrom raster nlayers getValues as.matrix ncol nrow stack
#' @importFrom stats quantile
#' @importFrom dplyr bind_rows
#' @export

visualise <- function(..., window = NULL, theme = gtTheme, trace = FALSE, image = FALSE,
                      new = TRUE, clip = TRUE){

  # check arguments ----
  window <- .testWindow(x = window, ...)
  assertDataFrame(x = window, nrows = 2, min.cols = 2, null.ok = TRUE)
  assertClass(x = theme, classes = "gtTheme", null.ok = TRUE)
  assertLogical(x = trace, len = 1, any.missing = FALSE)
  assertLogical(x = image, len = 1, any.missing = FALSE)
  assertLogical(x = new, len = 1, any.missing = FALSE)
  assertLogical(x = clip, len = 1, any.missing = FALSE)

  # derive the objects to plot
  objs <- rlang::enquos(...)

  names <- NULL
  objects <- list()
  for(i in seq_along(objs)){
    theObject <- theName <- NULL

    if(is.null(names(objs)[i]) || names(objs)[i] == ""){

      theObject <- eval_tidy(expr = objs[[i]])

      if(is.null(names(theObject))){
        theName <- NA
      } else if(image){
        theName <- "an image"
      } else {
        theName <- names(theObject)
      }

      if((class(theObject) == "RasterBrick" | class(theObject) == "RasterStack") & !image){
        temp <- lapply(1:dim(theObject)[3], function(x){
          theObject[[x]]
        })
        theObject <- temp
      } else if(class(theObject) == "matrix"){
        theObject <- list(theObject)
      }
    } else {
      if(!names(objs)[i] %in% names(theme@geom)){
        theObject <- eval_tidy(expr = objs[[i]])

        if((class(theObject) == "RasterBrick" | class(theObject) == "RasterStack") & !image){
          theName <- paste(names(objs)[i], 1:dim(theObject)[3])
          temp <- lapply(1:dim(theObject)[3], function(x){
            theObject[[x]]
          })
          theObject <- temp
        } else if(class(theObject) == "matrix"){
          theObject <- list(theObject)
          theName <- "a matrix"
        } else {
          theName <- names(objs)[i]
        }
      }
    }
    objects <- c(objects, theObject)
    names <- c(names, theName)
  }

  # plot already open? ----
  if(!is.null(dev.list()) & !new){
    objViewports <- grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    newPlot <- ifelse(any(objViewports$name == "vpLomm"), FALSE, TRUE)
    panelNames <- objViewports$name[objViewports$vpDepth == 2 & objViewports$name != "1"]
    panelNames <- panelNames[!duplicated(panelNames)]
    panels <- length(panelNames)
  } else{
    newPlot <- TRUE
    panels <- length(objects)
  }
  objects <- rep(x = objects, length.out = panels)
  names <- rep(x = names, length.out = panels)

  # checkup concerning plot size ----
  if(panels > 30){
    question <- readline(paste0("  -> this will produce ", panels, " plots, do you wish to continue? [yes/no]: "))
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
    pushViewport(viewport(name = "vpLomm"))
  }

  # plot the panels ----
  for(i in 1:panels){

    # make panel layout ----
    pnl <- makeLayout(x = objects[[i]],
                      window = window[i],
                      theme = theme)

    # make colours from theme for the object ----
    obj <- makeObject(x = objects[[i]],
                      image = image,
                      theme = theme,
                      ...)

    if(!is.na(names[[i]]) & !is.null(names[[i]])){
      plotName <- names[[i]]
    } else {
      plotName <- obj$name
    }

    if(newPlot | (!newPlot & obj$type == "raster")){

      # create the plot ----
      # open the panel viewport
      pushViewport(viewport(x = (panelPosX[i]/ncol)-(1/ncol/2),
                            y = (panelPosY[i]/nrow)-(1/nrow/2),
                            width = 1/ncol,
                            height = 1/nrow,
                            name = plotName))
      grid.rect(width = convertX(unit(1, "npc"), "native"),
                gp = gpar(col = NA, fill = NA), name = "panelGrob")
      grid.rect(height = pnl$yMargin, width = pnl$xMargin,
                gp = gpar(fill = NA, col = NA), name = "marginGrob")
      grid.rect(x = unit(pnl$minWinX, "points"), y = unit(pnl$minWinY, "points"),
                height = unit(pnl$maxWinY - pnl$minWinY, "points"),
                width = unit(pnl$maxWinX - pnl$minWinX, "points"),
                gp = gpar(fill = NA, col = NA), name = "extentGrob")

      pushViewport(viewport(x = unit(0.5, "npc") + unit(pnl$xOffset, "points"),
                            y = unit(0.5, "npc") + unit(pnl$yOffset, "points"),
                            height = min(pnl$gridH, pnl$gridHr),
                            width = min(pnl$gridW, pnl$gridWr),
                            name = "plot"))

      # the title viewport
      if(theme@title$plot){
        pushViewport(viewport(name = "title"))
        grid.text(just = "top",
                  y = unit(1, "npc") - unit(3, "points") + pnl$titleH,
                  label = plotName,
                  gp = gpar(fontsize = theme@title$fontsize,
                            col = theme@title$colour))
        upViewport() # exit title
      }

      # the yAxis viewport
      if(theme@yAxis$plot){
        pushViewport(viewport(xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                              yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
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
                    name = "ticks",
                    gp = gpar(fontsize = theme@yAxis$ticks$fontsize,
                              col = theme@yAxis$ticks$colour))
        }
        upViewport() # exit yAxis
      }

      # the xAxis viewport
      if(theme@xAxis$plot){
        pushViewport(viewport(xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                              yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
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
                    name = "ticks",
                    gp = gpar(fontsize = theme@xAxis$ticks$fontsize,
                              col = theme@xAxis$ticks$colour))
        }
        upViewport() # exit xAxis
      }

      # the legend viewport
      if(theme@legend$plot & obj$hasLegend){

        pushViewport(viewport(height = unit(1, "npc") * theme@legend$sizeRatio,
                              yscale = c(1, length(obj$legend$pos) + 0.1),
                              name = "legend"))

        grid.raster(x = unit(1, "npc") + unit(10, "points"),
                    width = unit(10, "points"),
                    height = unit(1, "npc"),
                    just = "left",
                    image = obj$uniqueValues$colours,
                    name = "theLegend",
                    interpolate = FALSE)

        if(theme@legend$box$plot){
          grid.rect(x = unit(1, "npc") + unit(10, "points"),
                    just = "left",
                    width = unit(1, "grobwidth", "theLegend"),
                    gp = gpar(col = theme@legend$box$colour,
                              fill = NA,
                              lty = theme@legend$box$linetype,
                              lwd = theme@legend$box$linewidth))
        }

        if(theme@legend$label$plot){
          grid.text(label = obj$legend$labels,
                    x = unit(1, "npc") + unit(1, "grobwidth", "theLegend") + unit(20, "points"),
                    y = obj$legend$pos,
                    just = c("left"),
                    gp = gpar(fontsize = theme@legend$label$fontsize,
                              col = theme@legend$label$colour))
        }

        # this is a little hack to get all the values that are contained in the
        # raster "into" the plotted object for later use (e.g. by locate())
        grid.text(label = obj$legend$labels,
                  name = "legendValues",
                  gp = gpar(col = NA))

        upViewport() # exit legend
      }

      # the grid viewport
      pushViewport(viewport(xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                            yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
                            name = "grid"))
      grid.rect(gp = gpar(col = NA, fill = NA), name = "gridGrob")

      # the grid viewport
      if(theme@grid$plot){

        # plot the major grid viewport
        pushViewport(viewport(xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                              yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
                              name = "majorGrid"))

        grid.grill(h = unit(pnl$yMajGrid, "native"),
                   v = unit(pnl$xMajGrid, "native"),
                   gp = gpar(col = theme@grid$colour,
                             lwd = theme@grid$linewidth,
                             lty = theme@grid$linetype))
        upViewport() # exit majorGrid

        # plot the minor grid
        if(theme@grid$minor){
          pushViewport(viewport(xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                                yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
                                name = "minorGrid"))

          grid.grill(h = unit(pnl$yMinGrid, "native"),
                     v = unit(pnl$xMinGrid, "native"),
                     gp = gpar(col = theme@grid$colour,
                               lwd = theme@grid$linewidth/2,
                               lty = theme@grid$linetype))
          upViewport() # exit minorGrid
        }
      }

      # the box viewport
      if(theme@box$plot){
        pushViewport(viewport(width = unit(1, "npc") - unit(2*pnl$xMargin, "native"),
                              height = unit(1, "npc") - unit(2*pnl$yMargin, "native"),
                              name = "box"))
        grid.rect(gp = gpar(fill = NA,
                            col = theme@box$colour,
                            lwd = theme@box$linewidth,
                            lty = theme@box$linetype),
                  name = "theBox")
        upViewport() # exit box
      }
      upViewport() # exit grid

      # the object viewport
      pushViewport(viewport(xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                            yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
                            name = "object"))
      grid.rect(gp = gpar(col = NA, fill = NA), name = "objectGrob")


      if(obj$type == "raster"){
        pushViewport(viewport(width = unit(1, "npc") - unit(2 * pnl$xMargin, "native") + unit(theme@box$linewidth, "points"),
                              height = unit(1, "npc") - unit(2 * pnl$yMargin, "native") + unit(theme@box$linewidth, "points"),
                              # xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                              # yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
                              name = "raster"))
        if(clip){
          grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "points"),
                    height = unit(1, "npc") + unit(theme@box$linewidth, "points"))
        }
        grid.raster(x = unit(0, "npc") - unit(pnl$xWindowOffset, "npc"),
                    y = unit(0, "npc") - unit(pnl$yWindowOffset, "npc"),
                    width = unit(1, "npc") * pnl$xFactor,
                    height = unit(1, "npc") * pnl$yFactor,
                    hjust = 0,
                    vjust = 0,
                    image = matrix(data = obj$array, nrow = obj$rows, ncol = obj$cols, byrow = TRUE),
                    name = "theRaster",
                    interpolate = FALSE)
      } else if(obj$type == "vector") {
        pushViewport(viewport(width = unit(1, "npc") - unit(2 * pnl$xMargin, "native") + unit(theme@box$linewidth, "points"),
                              height = unit(1, "npc") - unit(2 * pnl$yMargin, "native") + unit(theme@box$linewidth, "points"),
                              # xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                              # yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
                              name = "vector"))
        if(clip){
          grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "points"),
                    height = unit(1, "npc") + unit(theme@box$linewidth, "points"))
        }
        grid.draw(obj$out)
      }
      upViewport() # exit 'object'

      upViewport(3) # exit the object 'viewport' and 'plot' and 'plotName'

    } else {

      downViewport(panelNames[i])
      downViewport("plot")
      pushViewport(viewport(xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                            yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
                            name = "grid"))

      pushViewport(viewport(width = unit(1, "npc") - unit(2 * pnl$xMargin, "native"),
                            height = unit(1, "npc") - unit(2 * pnl$yMargin, "native"),
                            xscale = c(pnl$minWinX - pnl$xMargin, pnl$maxWinX + pnl$xMargin),
                            yscale = c(pnl$minWinY - pnl$yMargin, pnl$maxWinY + pnl$yMargin),
                            name = "geom"))

      if(clip){
        grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "points"),
                  height = unit(1, "npc") + unit(theme@box$linewidth, "points"))
      }
      grid.draw(obj$out)

      upViewport(4)

    }
  }
  upViewport() # exit 'vpLomm'

  if(trace){
    plotHistory <- FALSE

    for(i in seq_along(objects)){

      hasHistory <- ifelse(!is.null(tryCatch(expr = objects[[i]]@history, error = function(x) NULL)), TRUE, FALSE)

      if(hasHistory){

        if(grepl("RasterBrick", class(objects[[i]]))){
          theHistory <- lapply(seq_along(names(objects[[i]])), function(x){
            temp <- unlist(objects[[i]][[x]]@history)
          })
          if(!is.null(unlist(theHistory))){
            histMsg <- lapply(seq_along(names(objects[[i]])), function(x){
              paste0("the layer '", names(objects[[i]])[x], "' has the following history:\n -> ", paste0(theHistory[[x]], collapse = "\n -> "))
            })
            names(histMsg) <- names(objects[[i]])
            plotHistory <- TRUE
          }
        } else {
          theHistory <- unlist(objects[[i]]@history)
          if(!is.null(theHistory)){
            histMsg <- paste0("this object has the following history:\n -> ", paste0(theHistory, collapse = "\n -> "))
            plotHistory <- TRUE
          }
        }

      }
    }

    if(plotHistory){
      message(paste0(histMsg, collapse = "\n"))
    } else{
      message(paste0("this object has the following history:\n -> object loaded from memory"))
    }
  }

  invisible(recordPlot(attach = "geometr"))
}

#' Create a new theme
#'
#' Assign parameters in an gtTheme to create a new theme.
#' @param from [\code{gtTheme}]\cr an gtTheme object.
#' @param title [\code{named list(.)}]\cr \code{plot = TRUE/FALSE},
#'   \code{fontsize} and \code{colour} of the title.
#' @param box [\code{named list(.)}]\cr \code{plot = TRUE/FALSE},
#'   \code{linewidth}, \code{linetype} and \code{colour} of the bounding box
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
#' @param geom [\code{named list(.)}]\cr \code{line}, \code{fill},
#'   \code{linetype}, \code{linewidth}, \code{pointsize} and \code{pointsymbol}
#'   of a geom, \cr\cr scale [\code{named list(.)}]\cr \code{x =
#'   'someParameter'} and \code{to = 'someAttribute'} to which to scale
#'   'someParameter' to.
#' @param raster [ \code{named list(.)}]\cr \code{scale = 'someAttribute'} and
#'   at least two \code{colours} to which to scale 'someAttribute' to.
#' @examples
#' input <- gtRasters$continuous
#' (myTheme <- setTheme(title = list(plot = FALSE)))
#'
#' visualise(input, theme = myTheme)
#' @importFrom checkmate assertList assertLogical assertNames
#' @export

setTheme <- function(from = NULL, title = NULL, box = NULL, xAxis = NULL,
                     yAxis = NULL, grid = NULL, legend = NULL, geom = NULL,
                     raster = NULL){

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
    assertNames(names(box), subset.of = c("plot", "linewidth", "linetype", "colour"))
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
        assertList(xAxis$ticks, any.missing = FALSE, max.len = 4)
        assertNames(names(xAxis$ticks), subset.of = c("plot", "fontsize", "colour", "digits"))
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
        assertList(yAxis$ticks, any.missing = FALSE, max.len = 4)
        assertNames(names(yAxis$ticks), subset.of = c("plot", "fontsize", "colour", "digits"))
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

  assertList(geom, any.missing = FALSE, max.len = 7, null.ok = TRUE)
  if(!is.null(geom)){
    assertNames(names(geom), subset.of = c("scale", "linecol", "fillcol", "linetype", "linewidth", "pointsize", "pointsymbol"))
    previous <- from@geom
    for(i in seq_along(names(geom))){
      out@geom[which(names(previous) == names(geom)[i])] <- geom[i]
    }
  }

  assertList(raster, any.missing = FALSE, max.len = 2, null.ok = TRUE)
  if(!is.null(raster)){
    assertNames(names(raster), subset.of = c("scale", "colours"))
    previous <- from@raster
    for(i in seq_along(names(raster))){
      out@raster[which(names(previous) == names(raster)[i])] <- raster[i]
    }
  }

  return(out)
}
