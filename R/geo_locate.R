#' Locate (and identify) clicks
#'
#' Click into a plot to get the location or identify values
#' @param samples [integerish(1)][integer]\cr the number of clicks.
#' @param panel [character(1)][character]\cr the panel in which to locate (i.e. the
#'   title shown over the plot).
#' @param identify [logical(1)][logical]\cr get the raster value or \code{geom} ID
#'   at the sampled location (\code{TRUE}) or merely the location (\code{FALSE},
#'   default).
#' @param snap [logical(1)][logical]\cr should the returned value(s) be set to the
#'   nearest raster cell's center (\code{TRUE}) or should they remain the
#'   selected, "real" value (\code{FALSE}, default)?
#' @param raw [logical(1)][logical]\cr should the complete statistics about the
#'   clicks be returned (\code{TRUE}), or should only the basic output be
#'   returned (\code{FALSE}, default)?
#' @param show [logical(1)][logical]\cr should information be plotted
#'   (\code{TRUE}), or should they merely be returned to the console
#'   (\code{FALSE}, default)?
#' @param ... graphical parameters of the objects that are
#'   created when \code{show = TRUE}.
#' @return A \code{tibble} of the selected locations and, if \code{identify
#'   = TRUE}, the respective values. If \code{show = TRUE} the values are also
#'   shown in the plot.
#' @family geometry tools
#' @examples
#' if(dev.interactive()){
#'
#'   # locate coordinates with geoms
#'   geo_vis(geom = gtGeoms$polygon)
#'   geo_locate(samples = 2)
#'
#'   # locate or identify values with rasters
#'   geo_vis(raster = gtGeoms$grid)
#'   geo_locate(identify = TRUE, snap = TRUE)
#'
#'   # with several panels, specify a target
#'   geo_vis(gtGeoms$grid, gtGeoms$grid)
#'   geo_locate(samples = 4, panel = "categorical",
#'             snap = TRUE, identify = TRUE)
#'
#' }
#' @importFrom checkmate assertIntegerish assertCharacter assertLogical
#' @importFrom geomio subChrIntCpp pointInPolyCpp
#' @importFrom grDevices dev.list
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom grid grid.ls grid.grep grid.force seekViewport grid.locator gList
#'   pointsGrob textGrob grid.draw upViewport unit grid.get gPath grid.points
#' @importFrom raster as.matrix
#' @export

geo_locate <- function(samples = 1, panel = NULL, identify = FALSE, snap = FALSE,
                      raw = FALSE, show = TRUE, ...){

  # check arguments
  assertIntegerish(x = samples, lower = 1, max.len = 1)
  assertCharacter(x = panel, ignore.case = TRUE, len = 1, null.ok = TRUE)
  assertLogical(x = identify, len = 1)
  assertLogical(x = snap, len = 1)
  assertLogical(x = show, len = 1)

  # test whether a geometr plot is already open
  if(!is.null(dev.list())){
    objViewports <- grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    mainVP <- grid.grep("geometr", grobs = FALSE, viewports = TRUE, grep = TRUE)
    if(!ifelse(any(mainVP == "geometr"), TRUE, FALSE)){
      stop("please create a plot with geometr::geo_vis()")
    }

    panelNames <- objViewports$name[objViewports$vpDepth == unique(objViewports$vpDepth[which(objViewports$name == "geometr")]) + 1 & objViewports$name != "1"]
    panelNames <- panelNames[!duplicated(panelNames)]
  } else{
    stop("please create a plot with geometr::geo_vis()")
  }

  isLegendInPlot <- !identical(grid.grep("legend", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
  isRasterInPlot <- !identical(grid.grep("grid", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
  isVectorInPlot <- !identical(grid.grep("point|line|polygon", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))

  # get the panel in which locations should be determined
  if(is.null(panel)){
    if(length(panelNames) > 1){
      warning("please select locations in the first panel.", immediate. = TRUE, call. = FALSE)
    }
    panel <- panelNames[1]
  } else{
    panel <- panelNames[grepl(panel, panelNames)]
    if(length(panel) == 0){
      panel <- panelNames[1]
      warning("the specified panel did not match any of the existing panels (", paste0(panelNames, collapse = ", "), "), please select locations in the first panel.", immediate. = TRUE, call. = FALSE)
    }
  }

  # find the correct viewport to limit actions to this area of the plot
  if(isRasterInPlot){
    rasterVpPath <- grid.grep(paste0(panel, "::theLayout::grid"), grobs = FALSE, viewports = TRUE, grep = TRUE)
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
  if(isVectorInPlot){
    vectorVpPath <- grid.grep(paste0(panel, "::theLayout::point|line|polygon::box"), grobs = FALSE, viewports = TRUE, grep = TRUE)
    seekViewport(vectorVpPath)
  }

  extentGrobMeta <- grid.get(gPath("extentGrob"))
  panelExt <- tibble(x = c(as.numeric(extentGrobMeta$x), as.numeric(extentGrobMeta$x) + as.numeric(extentGrobMeta$width)),
                     y = c(as.numeric(extentGrobMeta$y), as.numeric(extentGrobMeta$y) + as.numeric(extentGrobMeta$height)))

  if(snap){
    theGrid <- tibble(x = rep(seq(panelExt$x[1] + 0.5, panelExt$x[2], 1), times = panelExt$y[2]),
                      xmin = rep(seq(panelExt$x[1], panelExt$x[2]-1), times = panelExt$y[2]),
                      xmax = rep(seq(panelExt$x[1]+1, panelExt$x[2]), times = panelExt$y[2]),
                      y = rep(seq(panelExt$y[2]-0.5, panelExt$y[1]), each = panelExt$x[2]),
                      ymin = rep(seq(panelExt$y[2]-1, panelExt$y[1]), each = panelExt$x[2]),
                      ymax = rep(seq(panelExt$y[2], panelExt$y[1]+1), each = panelExt$x[2]))
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
      matPos <- tibble(col = ceiling(matPos$x),
                       row = ceiling(matPos$y))
    } else{
      matPos <- tibble(col = ceiling(values[1]),
                       row = ceiling(values[2]))
    }

    temp <- tibble(fid = i, x = values[1], y = values[2])

    if(raw){
      temp <- bind_cols(temp, matPos)
    }

    if(identify & isLegendInPlot){
      metaLegend <- grid.get(gPath("legend_items"), global = TRUE)
      metaValues <- grid.get(gPath("legend_values"), global = TRUE)
      if(length(panelNames) > 1){
        theLegend <- metaLegend[which(panel == panelNames)][[1]]$raster
        theValues <- rev(as.numeric(metaValues[which(panel == panelNames)][[1]]$label))
      } else{
        theLegend <- metaLegend$raster
        theValues <- rev(as.numeric(metaValues$label))
      }
      if(isRasterInPlot){

        matVal <- subChrIntCpp(matCol,
                               replace = theLegend,
                               with = theValues)
        theCol <- matCol[dim(matCol)[1]+1 - matPos$row, matPos$col]
        theVal <- matVal[dim(matCol)[1]+1 - matPos$row, matPos$col]
        plotVal <- theVal
        vals <- tibble(value = theVal, colour = theCol)

      } else if(isVectorInPlot){

        theVal <- plotVal <- NA
        for(j in seq_along(unique(theValues))){
          geom <- grid.get(gPath(as.character(j)), global = TRUE)
          geom <- matrix(data = c(geom$x, geom$y, geom$pathId), ncol = 3)
          inside <- pointInPolyCpp(vert = matrix(data = c(values[1], values[2]), ncol = 2),
                                   geom = geom[which(geom[,3] == j), c(1, 2)],
                                   invert = FALSE)
          if(inside >= 1){
            theVal <- j
            plotVal <- j
            break
          }
        }
        vals <- tibble(geom = theVal)

      }
      temp <- bind_cols(temp, vals)
    }
    # return(click)

    if(show){
      if(identify){
        toDraw <- gList(pointsGrob(x = unit(values[1], "npc"),
                                   y = unit(values[2], "npc"),
                                   pch = 16,
                                   size = unit(1, "mm"),
                                   gp = gpar(...)),
                        textGrob(label = paste0("[", plotVal, "]"),
                                 x = unit(values[1], "npc") + unit(2, "mm"),
                                 y = unit(values[2], "npc"),
                                 just = "left",
                                 check.overlap = TRUE,
                                 gp = gpar(...))
        )
      } else{
        toDraw <- gList(pointsGrob(x = unit(values[1], "npc"),
                                   y = unit(values[2], "npc"),
                                   pch = 16,
                                   size = unit(1, "mm"),
                                   gp = gpar(...)
                                   ),
                        textGrob(label = paste0("(", values[1], ", ", values[2], ")"),
                                 x = unit(values[1], "npc") + unit(2, "mm"),
                                 y = unit(values[2], "npc"),
                                 just = "left",
                                 check.overlap = TRUE,
                                 gp = gpar(...))
                        )
      }
      grid.draw(toDraw)
    }
    out <- bind_rows(out, temp)

  }
  upViewport(4)

  return(out)
}