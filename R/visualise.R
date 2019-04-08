#' Visualise raster and geom objects
#'
#' @param raster [\code{Raster*} | \code{matrix}]\cr raster object to plot.
#' @param geom [\code{geom}]\cr Object of class \code{\link{geom}}.
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
#' @param ... [various]\cr graphical parameters to plot a \code{geom}.
#' @details In case you want to plot an image (simiar to
#'   \code{\link[raster]{plotRGB}}), you either have to: \enumerate{ \item
#'   provide a \code{RasterStack} with the three layers \code{red}, \code{green}
#'   and \code{blue} or \item provide a matrix with hexadecimal colour values
#'   (e.g. '#000000')} and set \code{image = TRUE}.
#'
#' @return Returns invisibly an object of class \code{recordedplot}, see
#'   \code{\link{recordPlot}} for details (and warnings).
#' @examples
#' # define a geometry
#' coords <- data.frame(x = c(30, 60, 60, 40),
#'                      y = c(40, 40, 60, 70),
#'                      fid = 1)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' (aGeom <- gs_polygon(anchor = coords, window = window, col = "blue"))
#'
#' # if plotted on top of an existing plot, the relative coordinate values
#' # will be used to construct the grob.
#' visualise(raster = gtRasters$continuous, geom = aGeom)
#' visualise(geom = aGeom)
#'
#' @importFrom checkmate testClass testList assertNames assertList assertLogical
#'   testCharacter testIntegerish testNames
#' @importFrom tibble tibble
#' @importFrom grid grid.newpage pushViewport viewport grid.rect grid.raster
#'   grid.clip unit grid.draw grid.grill upViewport grid.text gpar convertX
#'   downViewport
#' @importFrom grDevices colorRampPalette as.raster recordPlot rgb
#' @importFrom raster nlayers getValues as.matrix ncol nrow stack
#' @importFrom stats quantile
#' @export

visualise <- function(raster = NULL, geom = NULL, window = NULL, theme = gtTheme,
                      trace = FALSE, image = FALSE, new = TRUE, ...){

  # check arguments
  existsRaster <- !is.null(raster)
  existsGeom <- !is.null(geom)
  stopifnot(any(existsRaster, existsGeom))

  assertDataFrame(x = window, nrows = 2, min.cols = 2, null.ok = TRUE)
  if(!is.null(window)){
    assertNames(names(window), must.include = c("x", "y"))
    if(existsGeom){
      geom <- setWindow(x = geom, to = window)
    }
  }
  assertClass(x = theme, classes = "gtTheme", null.ok = TRUE)
  assertLogical(trace)
  assertLogical(image)
  assertLogical(new)

  # check whether a plot is already open and whether it is valid
  if(!is.null(dev.list()) & !new){
    objViewports <- grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    isOpenPlot <- ifelse(any(objViewports$name == "vpLomm"), TRUE, FALSE)

    panelNames <- objViewports$name[objViewports$vpDepth == 2 & objViewports$name != "1"]
    panelNames <- panelNames[!duplicated(panelNames)]
  } else{
    isOpenPlot <- FALSE
  }

  # turn raster into an array and extract meta-data
  if(existsRaster){

    isRaster <- any(grepl("Raster", class(raster)))
    if(isRaster){
      if(grepl("RasterBrick", class(raster))){
        raster <- stack(raster)
      }
    }
    isMatrix <- is.matrix(raster)
    if(isMatrix){
      if(!grepl("matrix", class(raster)[1]) & !image){
        warning("please provide a raw matrix in 'raster' or set 'image = TRUE'.", immediate. = T)
      }
    }

    if(isRaster){
      plotLayers <- nlayers(raster)
      griddedNames <- sapply(1:plotLayers, function(x){
        raster[[x]]@data@names
      })
      if(nchar(griddedNames[[1]]) == 0){
        griddedNames[[1]] <- "layer"
      }
      dims <- c(raster@nrows, raster@ncols)
      panelExt <- getExtent(raster)
    }

    if(isMatrix){
      plotLayers <- 1
      griddedNames <- "layer"
      dims <- dim(raster)
      panelExt <- tibble(x = c(0, ncol(raster)),
                         y = c(0, nrow(raster)))
    }

    # checks in case raster is supposed to be an "image"
    if(image){
      isRGB <- testNames(names(raster), permutation.of = c("red", "green", "blue"))
      isHex <- testCharacter(x = raster[1], pattern = "\\#(.{6,8})")

      if(isRGB){
        alpha <- rep(255, length(raster[[1]]))
        red <- getValues(raster[[which(griddedNames == "red")]])
        alpha[is.na(red)] <- 0L
        red[is.na(red)] <- 255L
        green <- getValues(raster[[which(griddedNames == "green")]])
        alpha[is.na(green)] <- 0L
        green[is.na(green)] <- 255L
        blue <- getValues(raster[[which(griddedNames == "blue")]])
        alpha[is.na(blue)] <- 0L
        blue[is.na(blue)] <- 255L
        theColours <- rgb(red = red, green = green, blue = blue, alpha = alpha, maxColorValue = 255)
      } else if(isHex){
        theColours <- as.vector(raster)
      } else{
        stop("please either provide rgb (in 3 layers) or hex values in 'raster'")
      }

      # meta stuff
      plotLayers <- 1
      griddedNames <- "image"
      theme@legend$plot <- FALSE

    }

    # set panelExt to window, if that exists
    if(!is.null(window)){
      window <- tibble(x = c(min(window$x), max(window$x)),
                       y = c(min(window$y), max(window$y)))
      xFactor <- (panelExt$x[2] - panelExt$x[1])/abs(window$x[2] - window$x[1])
      yFactor <- (panelExt$y[2] - panelExt$y[1])/abs(window$y[2] - window$y[1])
      xRasterOffset <- window$x[1] / abs(window$x[2] - window$x[1])
      yRasterOffset <- window$y[1] / abs(window$y[2] - window$y[1])
      panelExt <- window
    } else{
      xFactor <- yFactor <- 1
      xRasterOffset <- yRasterOffset <- 0
    }
  }

  if(isOpenPlot){
    if(existsRaster){
      # if both are given, check whether their names are the same. If not, prepare
      # to plot raster
      if(!all(panelNames == griddedNames)){
        isOpenPlot <- FALSE
        panelNames <- griddedNames
      }
    } else {
      plotLayers <- length(panelNames)
    }
  } else{
    if(existsRaster){
      panelNames <- griddedNames
    } else{
      panelNames <- geom@type
    }
  }

  # if plot still valid, determine which elements are available
  if(isOpenPlot){
    isLegendInPlot <- !identical(grid.grep("legend", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
    isRasterInPlot <- !identical(grid.grep("raster", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
    isGeomInPlot <-!identical(grid.grep("geom", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
  } else{
    isLegendInPlot <- FALSE
    isRasterInPlot <- FALSE
    isGeomInPlot <-FALSE
  }

  # turn 'geom' into a grob that can be plotted
  if(existsGeom){

    geom <- gt_scale(geom = geom, to = "relative")

    if(isOpenPlot){
      extentGrobMeta <- grid.get(gPath("extentGrob"))
      panelExt <- tibble(x = c(0, as.numeric(extentGrobMeta$width)) + as.numeric(extentGrobMeta$x),
                         y = c(0, as.numeric(extentGrobMeta$height)) + as.numeric(extentGrobMeta$y))
      geom <- setWindow(x = geom, to = panelExt)
    } else{
      if(!existsRaster){
        panelExt <- tibble(x = c(min(geom@window$x), max(geom@window$x)),
                           y = c(min(geom@window$y), max(geom@window$y)))
        plotLayers <- 1
      } else{
        geom <- setWindow(x = geom, to = panelExt)
      }
    }

    geomGrob <- gt_grob(input = geom, theme = theme, ...)
  }

  # checkup concerning plot size
  if(plotLayers > 30){
    question <- readline(paste0("  -> this will produce ", plotLayers, " plots, do you wish to continue? [yes/no]: "))
    question <- match.arg(question, c("yes", "no"))
    if(question == "no"){
      return(invisible(0))
    }
  }

  format <- makeFormat(panelExt = panelExt, theme = theme)

  if(isOpenPlot){
    if(isLegendInPlot){
      legendMeta <- grid.get(gPath("legendValues"))
      tickLabels <- as.numeric(legendMeta$label)
    }
  }

  # height and width of the plot elements
  if(theme@title$plot){
    titleH <- unit(theme@title$fontsize+6, units = "points")
  } else{
    titleH <- unit(0, "points")
  }
  if(theme@legend$plot){
    legendW <- ceiling(convertX(unit(1, "strwidth", as.character(100)) + unit(30, "points"), "points"))
  } else{
    legendW <- unit(0, "points")
  }
  if(theme@yAxis$plot){
    yAxisTitleW <- unit(theme@yAxis$label$fontsize+6, units = "points")
    yAxisTicksW <- ceiling(convertX(unit(1, "strwidth", as.character(max(round(format$yMajG, theme@yAxis$ticks$digits)))), "points"))
  } else{
    yAxisTitleW <- unit(0, "points")
    yAxisTicksW <- unit(0, "points")
  }
  if(theme@xAxis$plot){
    xAxisTitleH <- unit(theme@xAxis$label$fontsize+6, units = "points")
    xAxisTicksH <- unit(theme@xAxis$ticks$fontsize, units = "points")
  } else{
    xAxisTitleH <- unit(0, "points")
    xAxisTicksH <- unit(0, "points")
  }
  xOffset <- ((as.numeric(yAxisTicksW) + as.numeric(yAxisTitleW)) - as.numeric(legendW))/2
  yOffset <- ((as.numeric(xAxisTicksH) + as.numeric(xAxisTitleH)) - as.numeric(titleH))/2

  # determine the number of columns and rows and the position of panels
  if(plotLayers > 1){
    ncol <- ceiling(sqrt(plotLayers))
  } else{
    ncol <- 1
  }
  nrow <- ceiling(plotLayers/ncol)
  panelPosY <- rep(rev(seq(from = 1, to = nrow)), each = ncol)
  panelPosX <- rep(seq(from = 1, to = ncol), times = nrow)

  # create new plot and an overarching viewport
  if(!isOpenPlot){
    grid.newpage()
    pushViewport(viewport(name = "vpLomm"))
  }

  # start plotting the different elements
  for(i in 1:plotLayers){
    plotName <- panelNames[[i]]

    if(!isOpenPlot){

      # get colours for this panel
      if(!image){
        if(existsRaster){
          cls <- makeColours(input = raster[[i]], theme = theme, ...)
        } else if(existsGeom){
          cls <- makeColours(input = geom, theme = theme, ...)
          plotName <- as.character(cls$params$scale$cls)
        }
        theColours <- cls$out.cols
      }

      # the panel viewport
      pushViewport(viewport(x = (panelPosX[i]/ncol)-(1/ncol/2),
                            y = (panelPosY[i]/nrow)-(1/nrow/2),
                            width = 1/ncol,
                            height = 1/nrow,
                            name = plotName))
      grid.rect(width = convertX(unit(1, "npc"), "native"), gp = gpar(col = NA, fill = NA), name = "panelGrob")
      grid.rect(height = theme@yAxis$margin, width = theme@xAxis$margin,
                gp = gpar(fill = NA, col = NA), name = "marginGrob")
      grid.rect(x = unit(panelExt$x[1], "points"), y = unit(panelExt$y[1], "points"),
                height = unit(panelExt$y[2] - panelExt$y[1], "points"),
                width = unit(panelExt$x[2] - panelExt$x[1], "points"),
                gp = gpar(fill = NA, col = NA), name = "extentGrob")

      # determine dimensions for this plot
      gridH <- unit(1, "grobheight", "panelGrob") - xAxisTitleH - xAxisTicksH - titleH
      gridHr <- unit(1, "grobwidth", "panelGrob")*format$yRat - yAxisTitleW*format$yRat - yAxisTicksW*format$yRat - legendW*format$yRat
      gridW <- unit(1, "grobwidth", "panelGrob") - yAxisTitleW - yAxisTicksW - legendW
      gridWr <- unit(1, "grobheight", "panelGrob")*format$xRat - xAxisTitleH*format$xRat- xAxisTicksH*format$xRat - titleH*format$xRat

      pushViewport(viewport(x = unit(0.5, "npc") + unit(xOffset, "points"),
                            y = unit(0.5, "npc") + unit(yOffset, "points"),
                            height = min(gridH, gridHr),
                            width = min(gridW, gridWr),
                            name = "plot"))

      # the title viewport
      if(theme@title$plot){
        pushViewport(viewport(name = "title"))
        grid.text(just = "top",
                  y = unit(1, "npc") - unit(3, "points") + titleH,
                  label = plotName,
                  gp = gpar(fontsize = theme@title$fontsize,
                            col = theme@title$colour))
        upViewport() # exit title
      }

      # the legend viewport
      if(theme@legend$plot){

        pushViewport(viewport(height = unit(1, "npc")*theme@legend$sizeRatio,
                              yscale = c(1, length(cls$legend$values)+0.1),
                              name = "legend"))

        grid.raster(x = unit(1, "npc") + unit(10, "points"),
                    width = unit(10, "points"),
                    height = unit(1, "npc"),
                    just = "left",
                    image = cls$legend$array,
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
          grid.text(label = cls$legend$labels,
                    x = unit(1, "npc") + unit(1, "grobwidth", "theLegend") + unit(20, "points"),
                    y = cls$legend$labelsPos,
                    just = c("left"),
                    gp = gpar(fontsize = theme@legend$label$fontsize,
                              col = theme@legend$label$colour))
        }

        # this is a little hack to get all the values that are contained in the
        # raster "into" the plotted object for later use (e.g. by locate())
        grid.text(label = cls$legend$labels,
                  name = "legendValues",
                  gp = gpar(col = NA))

        upViewport() # exit legend
      }

      # the yAxis viewport
      if(theme@yAxis$label$plot){
        pushViewport(viewport(name = "yAxisTitle"))
        grid.text(just = "right",
                  x = unit(0, "npc") - unit(2, "points") - yAxisTicksW,
                  label = theme@yAxis$label$title,
                  rot = theme@yAxis$label$rotation,
                  gp = gpar(fontsize = theme@yAxis$label$fontsize,
                            col = theme@yAxis$label$colour))
        upViewport() # exit yAxisTitle
      }

      # the xAxis viewport
      if(theme@xAxis$label$plot){
        pushViewport(viewport(name = "xAxisTitle"))
        grid.text(just = "bottom",
                  y = unit(0, "npc") - unit(2, "points") - xAxisTitleH,
                  label = theme@xAxis$label$title,
                  rot = theme@xAxis$label$rotation,
                  gp = gpar(fontsize = theme@xAxis$label$fontsize,
                            col = theme@xAxis$label$colour))
        upViewport() # exit xAxisTitle
      }

      # the grid viewport
      pushViewport(viewport(xscale = c(panelExt$x[1]-format$xMar, panelExt$x[2]+format$xMar),
                            yscale = c(panelExt$y[1]-format$yMar, panelExt$y[2]+format$yMar),
                            name = "grid"))
      grid.rect(gp = gpar(col = NA, fill = NA), name = "gridGrob")

      if(theme@grid$plot){

        # the grid and axes viewport
        pushViewport(viewport(xscale = c(panelExt$x[1]-format$xMar, panelExt$x[2]+format$xMar),
                              yscale = c(panelExt$y[1]-format$yMar, panelExt$y[2]+format$yMar),
                              name = "majorGrid"))

        if(theme@xAxis$ticks$plot){
          grid.text(label = as.character(round(format$xMajG, theme@xAxis$ticks$digits)),
                    just = "top",
                    x = unit(format$xMajG, "native"),
                    y = unit(-0.005, "npc"),
                    name = "xAxisTicks",
                    gp = gpar(fontsize = theme@xAxis$ticks$fontsize,
                              col = theme@xAxis$ticks$colour))
        }
        if(theme@yAxis$ticks$plot){
          grid.text(label = as.character(round(format$yMajG, theme@yAxis$ticks$digits)),
                    just = "right",
                    x = unit(-0.005, "npc"),
                    y = unit(format$yMajG, "native"),
                    name = "yAxisTicks",
                    gp = gpar(fontsize = theme@yAxis$ticks$fontsize,
                              col = theme@yAxis$ticks$colour))
        }

        grid.grill(h = unit(format$yMajG, "native"),
                   v = unit(format$xMajG, "native"),
                   gp = gpar(col = theme@grid$colour,
                             lwd = theme@grid$linewidth,
                             lty = theme@grid$linetype))
        upViewport() # exit majorGrid

        # plot the minor grid
        if(theme@grid$minor){
          pushViewport(viewport(xscale = c(panelExt$x[1]-format$xMar, panelExt$x[2]+format$xMar),
                                yscale = c(panelExt$y[1]-format$yMar, panelExt$y[2]+format$yMar),
                                name = "minorGrid"))
          grid.grill(h = unit(format$yMinG, "native"),
                     v = unit(format$xMinG, "native"),
                     gp = gpar(col = theme@grid$colour,
                               lwd = theme@grid$linewidth/2,
                               lty = theme@grid$linetype))
          upViewport() # exit minorGrid
        }
      }

      # the box viewport
      if(theme@box$plot){
        pushViewport(viewport(width = unit(1, "npc") - unit(2*format$xMar, "native"),
                              height = unit(1, "npc") - unit(2*format$yMar, "native"),
                              name = "box"))
        grid.rect(gp = gpar(fill = NA,
                            col = theme@box$colour,
                            lwd = theme@box$linewidth,
                            lty = theme@box$linetype),
                  name = "theBox")
        upViewport() # exit box
      }

      # the raster viewport
      if(existsRaster){
        pushViewport(viewport(width = unit(1, "npc") - unit(2*format$xMar, "native") + unit(theme@box$linewidth, "points"),
                              height = unit(1, "npc") - unit(2*format$yMar, "native") + unit(theme@box$linewidth, "points"),
                              xscale = c(panelExt$x[1]-format$xMar, panelExt$x[2]+format$xMar),
                              yscale = c(panelExt$y[1]-format$yMar, panelExt$y[2]+format$yMar),
                              name = "raster"))
        grid.clip(width = unit(1, "npc") - unit(theme@box$linewidth, "points"),
                  height = unit(1, "npc") - unit(theme@box$linewidth, "points"))
        # return(c(unit(xRasterOffset, "npc"), unit(yRasterOffset, "npc")))
        grid.raster(x = unit(0, "npc") - unit(xRasterOffset, "npc"),
                    y = unit(0, "npc") - unit(yRasterOffset, "npc"),
                    width = unit(1, "npc")*xFactor,
                    height = unit(1, "npc")*yFactor,
                    hjust = 0,
                    vjust = 0,
                    image = matrix(data = theColours, nrow = dims[1], ncol = dims[2], byrow = TRUE),
                    name = "theRaster",
                    interpolate = FALSE)
        upViewport() # exit raster
      }

      # the geom viewport
      if(existsGeom){
        pushViewport(viewport(width = unit(1, "npc") - unit(2*format$xMar, "native"),
                              height = unit(1, "npc") - unit(2*format$yMar, "native"),
                              xscale = c(panelExt$x[1]-format$xMar, panelExt$x[2]+format$xMar),
                              yscale = c(panelExt$y[1]-format$yMar, panelExt$y[2]+format$yMar),
                              name = "geom"))
        grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "points"),
                  height = unit(1, "npc") + unit(theme@box$linewidth, "points"))
        grid.draw(geomGrob)
        upViewport() # exit geom
      }

      upViewport(3) # exit grid and plot and 'plotName'
    }

    if(isOpenPlot & existsGeom){

      # downViewport("vpLomm")
      downViewport(plotName)
      downViewport("plot")
      downViewport("grid")

      if(!isGeomInPlot){
        # grid.clip()
        pushViewport(viewport(width = unit(1, "npc") - unit(2*format$xMar, "native"),
                              height = unit(1, "npc") - unit(2*format$yMar, "native"),
                              # xscale = c(panelExt$x[1]-format$xMar, panelExt$x[2]+format$xMar),
                              # yscale = c(panelExt$y[1]-format$yMar, panelExt$y[2]+format$yMar),
                              name = "geom"))
      } else{
        downViewport("geom")
      }

      grid.draw(geomGrob)
      upViewport(4)
    }

  }

  if(!isOpenPlot){
    upViewport()
  } else if(isOpenPlot & existsGeom){
    upViewport()
  }

  if(trace){
    if(grepl("RasterBrick", class(raster))){
      theHistory <- lapply(seq_along(names(raster)), function(x){
        temp <- unlist(raster[[x]]@history)
      })
      if(!is.null(unlist(theHistory))){
        histMsg <- lapply(seq_along(names(raster)), function(x){
          paste0("the layer '", names(raster)[x], "' has the following history:\n -> ", paste0(theHistory[[x]], collapse = "\n -> "))
        })
        names(histMsg) <- names(raster)
        plotHistory <- TRUE
      } else{
        plotHistory <- FALSE
      }
    } else if(isRaster){
      theHistory <- unlist(raster@history)
      if(!is.null(theHistory)){
        histMsg <- paste0("this object has the following history:\n -> ", paste0(theHistory, collapse = "\n -> "))
        plotHistory <- TRUE
      } else{
        plotHistory <- FALSE
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
