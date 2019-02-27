#' Visualise raster and geom objects
#'
#' @param raster [\code{Raster*} | \code{matrix}]\cr raster object to plot.
#' @param geom [\code{geom}]\cr geom to plot.
#' @param window [\code{data.frame(1)}]\cr two oposing corners to which the plot
#'   is limited.
#' @param theme [\code{list(7)}]\cr Visualising options; see
#'   \code{\link{setTheme}} for details.
#' @param trace [\code{logical(1)}]\cr Print the raster object's history (i.e.
#'   the process according to which it has been created) (\code{TRUE}), or
#'   simply plot the object (\code{FALSE}, default).
#' @param image [\code{logical(1)}]\cr Does \code{raster} have the channels
#'   \code{red}, \code{green} and \code{blue}, i.e. is it an "image"
#'   (\code{TRUE}) or is this not the case (\code{FALSE}, default)?
#' @param new [\code{logical(1)}]\cr force a new plot (\code{TRUE}, default).
#' @param ... [various]\cr Graphical parameters to \code{geom}.
#' @details To create a plot with your own style, design it with
#'   \code{\link{setTheme}} and use it in \code{theme}.
#'
#'   In case you want to plot an image (simiar to
#'   \code{\link[raster]{plotRGB}}), you have to provide a \code{RasterStack} or
#'   \code{RasterBrick} with the three layers \code{red}, \code{green} and
#'   \code{blue} and set \code{image = TRUE}.
#' @return Returns invisibly an object of class \code{recordedplot}, see
#'   \code{\link{recordPlot}} for details (and warnings).
#' @examples
#' library(rasterTools)
#' input <- rtRasters$continuous
#' binarised <- rBinarise(input, thresh = 40)
#' visualise(raster = rDistance(binarised), trace = TRUE)
#'
#' # visualise also RasterBrick/-Stack objects
#' getDistances <- list(disEuc = list(operator = "rDistance"),
#'                      disMht = list(operator = "rDistance",
#'                                    method = "manhattan"),
#'                      disChb = list(operator = "rDistance",
#'                                    method = "chessboard"))
#' distances <- modify(input = binarised, by = getDistances, merge = TRUE)
#' distances <- raster::brick(binarised, distances)
#' visualise(distances)
#'
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
#' visualise(raster = input, geom = aGeom)
#' visualise(geom = aGeom)
#'
#' @importFrom checkmate testClass testList assertNames assertList assertLogical
#'   testCharacter testIntegerish
#' @importFrom tibble tibble
#' @importFrom grid grid.newpage pushViewport viewport grid.rect grid.raster
#'   grid.clip unit grid.draw grid.grill upViewport grid.text gpar convertX
#'   downViewport
#' @importFrom grDevices colorRampPalette as.raster recordPlot rgb
#' @importFrom raster nlayers values as.matrix ncol nrow
#' @importFrom stats quantile
#' @export

visualise <- function(raster = NULL, geom = NULL, window = NULL, theme = gtTheme,
                      trace = FALSE, image = FALSE, new = TRUE, ...){

  # check arguments
  if(!is.null(raster)){
    existsRaster <- TRUE
    isRaster <- grepl("Raster", class(raster))
  } else{
    existsRaster <- FALSE
  }
  existsGeom <- !is.null(geom)
  if(!existsGeom & !is.null(geom)){
    stop("please provide a valid 'geom' object to plot.")
  }
  stopifnot(any(existsRaster, existsGeom))
  if(!is.null(window) & existsGeom){
    geom <- setWindow(x = geom, to = window)
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

    if(isRaster){

      plotLayers <- nlayers(raster)
      griddedNames <- raster@data@names
      vals <- lapply(1:plotLayers, function(x){
        raster[[x]]@data@values
      })
      uniqueVals <- lapply(1:plotLayers, function(x){
        sortUniqueC(vals[[x]][!is.na(vals[[x]])])
      })
      dims <- c(raster@nrows, raster@ncols)
      ext <- raster[[1]]@extent
      panelExt <- tibble(x = c(ext@xmin, ext@xmax),
                         y = c(ext@ymin, ext@ymax))
      hasColourTable <- lapply(1:plotLayers, function(x){
        as.logical(length(raster[[x]]@legend@colortable))
      })
      isFactor <- lapply(1:plotLayers, function(x){
        raster[[x]]@data@isfactor
      })

    } else{

      plotLayers <- 1
      griddedNames <- "layer"
      vals <- list(getValuesMatC(raster))
      uniqueVals <- list(sort(unique(vals[[1]], na.rm = TRUE)))
      dims <- dim(raster)
      panelExt <- tibble(x = c(0, ncol(raster)),
                         y = c(0, nrow(raster)))
      hasColourTable <- FALSE
      isFactor <- FALSE

    }

    # checks in case raster is supposed to be an "image"
    if(image){
      assertNames(griddedNames, permutation.of = c("red", "green", "blue"))
      assertIntegerish(plotLayers, lower = 3, upper = 3)
      plotLayers <- 1
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

  # override legend in some cases
  if(isGeomInPlot & !isRasterInPlot | image){
    theme@legend$plot <- FALSE
  }

  # turn 'geom' into a grob that can be plotted
  if(existsGeom){

    if(isOpenPlot){
      extentGrobMeta <- grid.get(gPath("extentGrob"))
      panelExt <- tibble(x = c(0, as.numeric(extentGrobMeta$width)) + as.numeric(extentGrobMeta$x),
                         y = c(0, as.numeric(extentGrobMeta$height)) + as.numeric(extentGrobMeta$y))
    } else{
      if(!existsRaster){
        panelExt <- tibble(x = c(min(geom@window$x), max(geom@window$x)),
                           y = c(min(geom@window$y), max(geom@window$y)))
        plotLayers <- 1
        panelNames <- geom@type
      }
    }

    geom <- setWindow(x = geom, to = panelExt)
    geomGrob <- gt_as_grob(geom = geom, theme = theme, ...)

  }

  # checkup concerning plot size
  if(plotLayers > 30){
    question <- readline(paste0("  -> this will produce ", plotLayers, " plots, do you wish to continue? [yes/no]: "))
    question <- match.arg(question, c("yes", "no"))
    if(question == "no"){
      return(invisible(0))
    }
  }

  # manage plot properties
  ratio <- list(x = (panelExt$x[2] - panelExt$x[1])/(panelExt$y[2] - panelExt$y[1]),
                y = (panelExt$y[2] - panelExt$y[1])/(panelExt$x[2] - panelExt$x[1]))
  xBins <- theme@xAxis$bins
  yBins <- theme@yAxis$bins
  xBinSize <- (panelExt$x[2] - panelExt$x[1])/xBins
  yBinSize <- (panelExt$y[2] - panelExt$y[1])/yBins
  axisSteps <- list(x1 = seq(from = panelExt$x[1],
                             to = panelExt$x[2],
                             by = (panelExt$x[2] - panelExt$x[1])/xBins),
                    x2 = seq(from = panelExt$x[1] + (xBinSize/2),
                             to = panelExt$x[2],
                             by = (panelExt$x[2] - panelExt$x[1])/xBins),
                    y1 = seq(from = panelExt$y[1],
                             to = panelExt$y[2],
                             by = (panelExt$y[2] - panelExt$y[1])/yBins),
                    y2 = seq(from = panelExt$y[1] + (yBinSize/2),
                             to = panelExt$y[2],
                             by = (panelExt$y[2] - panelExt$y[1])/yBins))
  margin <- list(x = (panelExt$x[2]-panelExt$x[1])*theme@yAxis$margin,
                 y = (panelExt$y[2]-panelExt$y[1])*theme@xAxis$margin)

  # manage the colours
  if(existsRaster){

    if(image){
      red <- as.integer(vals[[which(panelNames == "red")]])
      red[is.na(red)] <- 255L
      green <- as.integer(vals[[which(panelNames == "green")]])
      green[is.na(green)] <- 255L
      blue <- as.integer(vals[[which(panelNames == "blue")]])
      blue[is.na(blue)] <- 255L
      theColours <- list(rgb(red = red, green = green, blue = blue, maxColorValue = 255))
      panelNames <- "image"
    } else{
      uniqueColours <- lapply(seq_along(uniqueVals), function(x){
        tempVals <- uniqueVals[[x]]
        nrVals <- length(tempVals)
        if(tempVals[1] == 0){
          tempVals <- tempVals+1
        }
        if(nrVals < 256){
          nrColours <- nrVals
        } else{
          nrColours <- 256
        }
        if(hasColourTable[[x]]){
          raster[[x]]@legend@colortable[tempVals]
        } else{
          colorRampPalette(colors = theme@raster$colours)(nrColours)
        }
      })

      # if(theme@legend$common){
      #   uniqueVals <- lapply(seq_along(uniqueVals), function(x){
      #     sort(unique(unlist(uniqueVals), na.rm = TRUE))
      #
      #   })
      # }

      theColours <- lapply(seq_along(uniqueVals), function(x){
        tempVals <- uniqueVals[[x]]
        nrVals <- length(tempVals)
        if(nrVals == 1){
          if(tempVals == 0){
            tempVals <- 1
          }
        }
        if(nrVals < 256){
          nrColours <- nrVals
        } else{
          nrColours <- 256
        }

        if(hasColourTable[[x]]){
          breaksTemp <- c(tempVals[1]-1, tempVals)
        } else if(isFactor[[x]]){
          attr <- raster[[x]]@data@attributes[[1]]
          idPos <- grep("id", colnames(attr), ignore.case = TRUE)
          breaksTemp <- c(tempVals[1]-1, attr[,idPos])
        } else{
          breaksTemp <- c(tempVals[1]-1, seq(tempVals[1], tempVals[[length(tempVals)]], length.out = nrColours))
        }
        valCuts <- cut(vals[[x]], breaks = breaksTemp, include.lowest = TRUE)
        uniqueColours[[x]][valCuts]
      })
    }

    # colours for the legend
    tickValues <- lapply(seq_along(uniqueVals), function(x){
      if(length(uniqueVals[[x]]) > theme@legend$bins){
        quantile(uniqueVals[[x]], probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
      } else{
        uniqueVals[[x]]
      }
    })
    tickLabels <- lapply(seq_along(uniqueVals), function(x){
      round(tickValues[[x]], 1)
    })

  } else if(isOpenPlot){
    if(isLegendInPlot){
      legendMeta <- grid.get(gPath("legendValues"))
      tickLabels <- as.numeric(legendMeta$label)
    }
  } else{
    theme@legend$plot <- FALSE
  }

  # height and width of the plot elements
  if(theme@title$plot){
    titleH <- unit(theme@title$fontsize+6, units = "points")
  } else{
    titleH <- unit(0, "points")
  }
  if(theme@legend$plot){
    legendW <- ceiling(convertX(unit(1, "strwidth", as.character(max(unlist(tickLabels)))) + unit(30, "points"), "points"))
  } else{
    legendW <- unit(0, "points")
  }
  if(theme@yAxis$plot){
    yAxisTitleW <- unit(theme@yAxis$label$fontsize+6, units = "points")
    yAxisTicksW <- ceiling(convertX(unit(1, "strwidth", as.character(max(round(axisSteps$y1, theme@yAxis$ticks$digits)))), "points"))
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
    plotName <- panelNames[i]

    if(!isOpenPlot){

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
      gridHr <- unit(1, "grobwidth", "panelGrob")*ratio$y - yAxisTitleW*ratio$y - yAxisTicksW*ratio$y - legendW*ratio$y
      gridW <- unit(1, "grobwidth", "panelGrob") - yAxisTitleW - yAxisTicksW - legendW
      gridWr <- unit(1, "grobheight", "panelGrob")*ratio$x - xAxisTitleH*ratio$x- xAxisTicksH*ratio$x - titleH*ratio$x

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

        if(length(tickValues[[i]]) < 2){
          pushViewport(viewport(height = unit(1, "npc")*theme@legend$sizeRatio,
                                yscale = c(1, length(uniqueVals[[i]])+0.1),
                                name = "legend"))
        } else{
          pushViewport(viewport(height = unit(1, "npc")*theme@legend$sizeRatio,
                                yscale = c(1, length(uniqueVals[[i]])+0.1),
                                name = "legend"))
        }

        # order the legend
        if(theme@legend$ascending){
          theLegend <- matrix(data = rev(uniqueColours[[i]]), ncol = 1, nrow = length(uniqueColours[[i]]))
          theValues <- rev(uniqueVals[[i]])
          valPos <- unit(which(uniqueVals[[i]] %in% tickValues[[i]]), "native")
        } else{
          theLegend <- matrix(data = uniqueColours[[i]], ncol = 1, nrow = length(uniqueColours[[i]]))
          theValues <- uniqueVals[[i]]
          valPos <- rev(unit(which(uniqueVals[[i]] %in% tickValues[[i]]), "native"))
        }

        grid.raster(x = unit(1, "npc") + unit(10, "points"),
                    width = unit(10, "points"),
                    height = unit(1, "npc"),
                    just = "left",
                    image = theLegend,
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
          grid.text(label = tickLabels[[i]],
                    x = unit(1, "npc") + unit(1, "grobwidth", "theLegend") + unit(20, "points"),
                    y = valPos,
                    just = c("left"),
                    gp = gpar(fontsize = theme@legend$label$fontsize,
                              col = theme@legend$label$colour))
        }

        # this is a little hack to get all the values that are contained in the
        # raster "into" the plotted object for later use (e.g. by locate())
        grid.text(label = theValues,
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
      pushViewport(viewport(xscale = c(panelExt$x[1]-margin$x, panelExt$x[2]+margin$x),
                            yscale = c(panelExt$y[1]-margin$y, panelExt$y[2]+margin$y),
                            name = "grid"))
      grid.rect(gp = gpar(col = NA, fill = NA), name = "gridGrob")

      if(theme@grid$plot){

        # the grid and axes viewport
        pushViewport(viewport(xscale = c(panelExt$x[1]-margin$x, panelExt$x[2]+margin$x),
                              yscale = c(panelExt$y[1]-margin$y, panelExt$y[2]+margin$y),
                              name = "majorGrid"))

        if(theme@xAxis$ticks$plot){
          grid.text(label = as.character(round(axisSteps$x1, theme@xAxis$ticks$digits)),
                    just = "top",
                    x = unit(axisSteps$x1, "native"),
                    y = unit(-0.005, "npc"),
                    name = "xAxisTicks",
                    gp = gpar(fontsize = theme@xAxis$ticks$fontsize,
                              col = theme@xAxis$ticks$colour))
        }
        if(theme@yAxis$ticks$plot){
          grid.text(label = as.character(round(axisSteps$y1, theme@yAxis$ticks$digits)),
                    just = "right",
                    x = unit(-0.005, "npc"),
                    y = unit(axisSteps$y1, "native"),
                    name = "yAxisTicks",
                    gp = gpar(fontsize = theme@yAxis$ticks$fontsize,
                              col = theme@yAxis$ticks$colour))
        }

        grid.grill(h = unit(axisSteps$y1, "native"),
                   v = unit(axisSteps$x1, "native"),
                   gp = gpar(col = theme@grid$colour,
                             lwd = theme@grid$linewidth,
                             lty = theme@grid$linetype))
        upViewport() # exit majorGrid

        # plot the minor grid
        if(theme@grid$minor){
          pushViewport(viewport(xscale = c(panelExt$x[1]-margin$x, panelExt$x[2]+margin$x),
                                yscale = c(panelExt$y[1]-margin$y, panelExt$y[2]+margin$y),
                                name = "minorGrid"))
          grid.grill(h = unit(axisSteps$y2, "native"),
                     v = unit(axisSteps$x2, "native"),
                     gp = gpar(col = theme@grid$colour,
                               lwd = theme@grid$linewidth/2,
                               lty = theme@grid$linetype))
          upViewport() # exit minorGrid
        }
      }

      # the box viewport
      if(theme@box$plot){
        pushViewport(viewport(width = unit(1, "npc") - unit(2*margin$x, "native"),
                              height = unit(1, "npc") - unit(2*margin$y, "native"),
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
        pushViewport(viewport(width = unit(1, "npc") - unit(2*margin$x, "native"),
                              height = unit(1, "npc") - unit(2*margin$y, "native"),
                              xscale = c(panelExt$x[1]-margin$x, panelExt$x[2]+margin$x),
                              yscale = c(panelExt$y[1]-margin$y, panelExt$y[2]+margin$y),
                              name = "raster"))
        grid.raster(width = unit(1, "npc"),
                    height = unit(1, "npc"),
                    image = matrix(data = theColours[[i]], nrow = dims[1], ncol = dims[2], byrow = TRUE),
                    name = "theRaster",
                    interpolate = FALSE)
        upViewport() # exit raster
      }

      # the geom viewport
      if(existsGeom){
        pushViewport(viewport(width = unit(1, "npc") - unit(2*margin$x, "native"),
                              height = unit(1, "npc") - unit(2*margin$y, "native"),
                              xscale = c(panelExt$x[1]-margin$x, panelExt$x[2]+margin$x),
                              yscale = c(panelExt$y[1]-margin$y, panelExt$y[2]+margin$y),
                              name = "geom"))
        grid.clip(width = unit(1, "npc") + unit(theme@box$linewidth, "native"),
                  height = unit(1, "npc") + unit(theme@box$linewidth, "native"))
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
        pushViewport(viewport(width = unit(1, "npc") - unit(2*margin$x, "native"),
                              height = unit(1, "npc") - unit(2*margin$y, "native"),
                              # xscale = c(panelExt$x[1]-margin$x, panelExt$x[2]+margin$x),
                              # yscale = c(panelExt$y[1]-margin$y, panelExt$y[2]+margin$y),
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
    if(grepl("RasterBrick", class(distances))){
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
#' input <- rtRasters$continuous
#' myTheme <- setTheme(geom = list(pointsize = .5, pointsymbol = 21, linewidth = 1,
#'                                 fill = c("#00204D", "#FFEA46"), line = "grey",
#'                                 scale = list(x = "fill", to = "inequality")))
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
    assertNames(names(geom), subset.of = c("scale", "line", "fill", "linetype", "linewidth", "pointsize", "pointsymbol"))
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
