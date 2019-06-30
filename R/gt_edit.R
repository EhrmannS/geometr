#' Relocate the vertices of geometries
#'
#' \code{gt_edit} allows to edit the vertices of any \code{geom}.
#' @param panel [\code{character(1)}]\cr the panel in which to edit vertices
#'   (i.e. the title shown over the plot).
#' @param tolerance [\code{numeric(1)}]\cr width of the locator-boxes as
#'   proportion of the overall box width.
#' @param fid [\code{integerish(1)}]\cr the feature to edit.
#' @param verbose [\code{logical(1)}]\cr be verbose about intermediate steps?
#' @return An invisible \code{geom}.
#' @family geometry tools
#' @examples
#' \dontrun{
#' library(sf)
#'
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' nc_geom <- gc_geom(input = nc)
#' visualise(`North Carolina` = nc_geom, window = data.frame(x = c(-82.1, -81), y = c(36.6, 35.9)))
#' modGeom <- gt_edit()
#' }
#' @importFrom grid grid.grep gPath grid.edit grid.remove
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom methods as
#' @export

gt_edit <- function(panel = NULL, tolerance = 0.01, fid = NULL, verbose = FALSE){

  # check arguments
  assertCharacter(x = panel, ignore.case = TRUE, len = 1, null.ok = TRUE)
  assertNumeric(x = tolerance, upper = 1, lower = 0.001, finite = TRUE)
  assertIntegerish(x = fid, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = verbose, any.missing = FALSE, len = 1)

  # function to make little squares (locator boxes) around all points that help
  # the user click on the vertices
  makeLocatorBoxes <- function(x = NULL, y = NULL, window = NULL){

    theVerts <- data.frame(x = as.numeric(x) * (max(window$x) - min(window$x)),
                           y = as.numeric(y) * (max(window$y) - min(window$y)))

    squareSize <- (max(window$x) - min(window$x)) * tolerance
    cols <- colorRampPalette(gtTheme@geom$linecol)(length(x))

    for(i in seq_along(x)){
      tempVerts <- theVerts[i, ]
      temp <- data.frame(fid = i,
                         x = c(tempVerts$x - squareSize, tempVerts$x - squareSize, tempVerts$x + squareSize, tempVerts$x + squareSize, tempVerts$x - squareSize)/(max(window$x) - min(window$x)),
                         y = c(tempVerts$y - squareSize, tempVerts$y + squareSize, tempVerts$y + squareSize, tempVerts$y - squareSize, tempVerts$y - squareSize)/(max(window$y) - min(window$y)))
      if(i == 1){
        rects <- pathGrob(x = unit(temp$x, "npc"),
                          y = unit(temp$y, "npc"),
                          id = as.numeric(as.factor(temp$fid)),
                          rule = "evenodd",
                          name = paste0("locBox", unique(temp$fid)),
                          gp = gpar(
                            col = cols[i], fill = "grey"))
      } else{
        rects <- gList(rects,
                       pathGrob(x = unit(temp$x, "npc"),
                                y = unit(temp$y, "npc"),
                                id = as.numeric(as.factor(temp$fid)),
                                rule = "evenodd",
                                name = paste0("locBox", unique(temp$fid)),
                                gp = gpar(
                                  col = cols[i], fill = "grey")))
      }
    }
    return(rects)
  }

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

  isGeomInPlot <- !identical(grid.grep("geom", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))

  if(isGeomInPlot){
    geomVpPath <- grid.grep(paste0(panel, "::plot::grid::geom"), grobs = FALSE, viewports = TRUE, grep = TRUE)
  } else {
    stop("please plot a valid 'geom' that can be edited.")
  }

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

  # get meta data for the target grob
  extentGrob <- grid.get(gPath("extentGrob"))
  theWindow <- tibble(x = rep(c(as.numeric(extentGrob$x), as.numeric(extentGrob$x) + as.numeric(extentGrob$width)), each = 2),
                      y = c(as.numeric(extentGrob$y), as.numeric(extentGrob$y) + as.numeric(extentGrob$height), as.numeric(extentGrob$y) + as.numeric(extentGrob$height), as.numeric(extentGrob$y)))

  # get list of geoms in the plot
  theGrobs <- grid.grep(gPath("^\\d*$"), grep = TRUE, global = TRUE)
  if(!is.null(fid)){
    temp <- unlist(lapply(seq_along(theGrobs), function(x){
      as.numeric(theGrobs[x][[1]]$name) %in% fid
    }))
    theGrobs <- theGrobs[temp]
  }

  # if there is more than one grob, ask the user to click in the panel to chose the 'targetGrob'
  if(length(theGrobs) > 1){

    targetGrobN <- NULL

    while(is.null(targetGrobN)){
      message("press click into the object you want to edit.")
      targetClick <- gt_locate(samples = 1, panel = panelNames, silent = TRUE)
      for(i in seq_along(theGrobs)){
        aGrob <- grid.get(gPath(as.character(i)), global = TRUE)
        aGrob <- data.frame(x = min(theWindow$x) + (as.numeric(aGrob$x) * (max(theWindow$x) - min(theWindow$x))),
                            y = min(theWindow$y) + (as.numeric(aGrob$y) * (max(theWindow$y) - min(theWindow$y))))
        inTargetGrob <- pointInGeomC(vert = as.matrix(targetClick[c("x", "y")]),
                                     geom = as.matrix(aGrob),
                                     invert = FALSE)
        if(inTargetGrob){
          targetGrobN <- as.character(i)
        }
      }
      if(is.null(targetGrobN)){
        message("I did not match an object, please click again.")
      } else {
        message("  -> object with fid = '", targetGrobN,"' selected.")
      }
    }
  } else {
    targetGrobN <- as.character(fid)
  }
  targetGrob <- grid.get(gPath(targetGrobN), global = TRUE)

  # create locator boxes
  rectangles <- makeLocatorBoxes(x = targetGrob$x, y = targetGrob$y, window = theWindow)
  seekViewport(geomVpPath)
  grid.draw(rectangles)

  message("press [Esc] (twice) to end editing.")
  while(TRUE) {

    targetRect <- NULL

    while(is.null(targetRect)) {
      firstClick <- gt_locate(samples = 1, panel = panelNames, silent = TRUE)
      if(all(is.na(firstClick[c("x", "y")]))){
        break
      } else {
        if(verbose){
          cat(paste0("old: ", firstClick$x, ",", firstClick$y, "  "))
        }
      }
      for(i in seq_along(rectangles)){
        tempCoords <- data.frame(x = min(theWindow$x) + (as.numeric(rectangles[[i]]$x) * (max(theWindow$x) - min(theWindow$x))),
                                 y = min(theWindow$y) + (as.numeric(rectangles[[i]]$y) * (max(theWindow$y) - min(theWindow$y))))
        # when editing a vertex in gt_sf(nc), no locator box is matched, which is probably due to c++
        inRect <- pointInGeomC(vert = as.matrix(firstClick[c("x", "y")]), geom = as.matrix(tempCoords), invert = FALSE)
        if(inRect){
          targetRect <- i
        }
      }
      if(is.null(targetRect)){
        message("I did not match a locator box, please click again.")
      }
    }

    secondClick <- gt_locate(samples = 1, panel = panelNames, silent = TRUE)
    if(all(is.na(secondClick[c("x", "y")]))){
      break
    } else {
      if(verbose){
        cat(paste0("-> new: ", secondClick$x, ",", secondClick$y, "\n"))
      }

      tempX <- unit(x = abs(min(theWindow$x) - secondClick$x)/(max(theWindow$x) - min(theWindow$x)), units = "npc")
      tempY <- unit(x = abs(min(theWindow$y) - secondClick$y)/(max(theWindow$y) - min(theWindow$y)), units = "npc")

      targetGrob$x[targetRect] <- tempX
      targetGrob$y[targetRect] <- tempY
      grid.edit(gPath(targetGrobN),
                x = targetGrob$x,
                y = targetGrob$y, global = TRUE)
      rectangles <- makeLocatorBoxes(x = targetGrob$x, y = targetGrob$y, window = theWindow)
      grid.edit(gPath(paste0("locBox", targetRect)),
                x = rectangles[[targetRect]]$x,
                y = rectangles[[targetRect]]$y)
    }

  }
  for(i in seq_along(rectangles)){
    grid.remove(gPath(paste0("locBox", i)))
  }

  newGrob <- grid.get(gPath(targetGrobN))
  newGeom <- gc_grob(input = newGrob, window = extentGrob)

  invisible(newGeom)

}