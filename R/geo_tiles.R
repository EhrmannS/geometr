#' Create a regular tiling \code{geom}
#'
#' Create a regular tiling polygon geometry for the extent of an crds value.
#' @param extent [data.frame(2)][data.frame]\cr Coordinates of the extent within
#'   which to build the tiles from. It must include the column names \code{x}
#'   and \code{y}.
#' @param width [numeric(1)][numeric]\cr the width (which does not correspond to
#'   the height in case of \code{pattern = "hexagonal"}) of a tile.
#' @param pattern [character(1)][character]\cr pattern of the tiling. Possible
#'   options are \code{"squared"} (default) or \code{"hexagonal"}.
#' @param centroids [logical(1)][logical]\cr should the centroids of the tiling
#'   be returned (\code{TRUE}) or should the tiling be returned (\code{FALSE},
#'   default)?
#' @param origin from which of the four corners to start numbering features?
#'   Options are \code{topleft}, \code{bottomleft} (default), \code{topright}
#'   and \code{bottomright}.
#' @details When deriving a regular tiling for a prescribed window, there is
#'   only a limited set of legal combinations of cells in x and y dimension. For
#'   instance, a window of 100 by 100 can't comprise 10 by 5 squares of
#'   side-length/width 10, because then the y-dimension wouldn't be fully
#'   covered. The same is true for hexagonal and triangular tilings.
#' @return A \code{geom}.
#' @family tilings
#' @examples
#' # create a squared tiling
#' tileExt <- data.frame(x = c(-180, 180),
#'                       y = c(-60, 80))
#' tiles <- geo_tiles(extent = tileExt, width = 10)
#' geo_vis(`10° world tiles` = tiles)
#'
#' # create a hexagonal tiling on top of a geom
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#'
#' aGeom <- geo_polygon(crds = coords, window = window)
#' geo_vis(`honeycomb background` = aGeom)
#'
#' hex <- geo_tiles(extent = geomio::getExtent(aGeom), width = 8, pattern = "hexagonal")
#' geo_vis(hex, linecol = "deeppink", new = FALSE)
#' @importFrom checkmate testDataFrame assertNames testClass testIntegerish
#'   assertDataFrame assertNames assertCharacter assertSubset assertLogical
#' @importFrom geomio pointInPolyCpp
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows group_by summarise left_join select mutate
#' @export

geo_tiles <- function(extent = NULL, width = NULL, pattern = "squared",
                      centroids = FALSE, origin = "bottomleft"){

  # check arguments
  assertDataFrame(x = extent, types = "numeric", any.missing = FALSE, ncols = 2, nrows = 2, null.ok = TRUE, .var.name = "extent->cols(x)")
  if(!is.null(extent)) assertNames(x = colnames(extent), permutation.of = c("x", "y"), .var.name = "extent->names(x)")
  assertIntegerish(x = width, len = 1)
  assertChoice(x = pattern, choices = c("squared", "hexagonal", "triangular"))
  assertChoice(x = origin, choices = c("topleft", "bottomleft", "topright", "bottomright"))
  assertLogical(x = centroids)


  # set pattern specific properties
  if(pattern == "squared"){

    offset <- width*0.5*-1
    nPoints <- 5

    # determine centroids
    xCentroids <- seq(min(extent$x) - offset, max(extent$x) + width + offset, by = width)
    yCentroids <- seq(min(extent$y) - offset, max(extent$y) + width + offset, by = width)
    if(origin == "topleft"){
      yCentroids <- rev(yCentroids)
    } else if(origin == "topright") {
      xCentroids <- rev(xCentroids)
    } else if(origin == "bottomright") {
      xCentroids <- rev(xCentroids)
      yCentroids <- rev(yCentroids)
    }
    cntrds <- tibble(x = rep(xCentroids, times = length(yCentroids)),
                     y = rep(yCentroids, each = length(xCentroids)))
    targetCentroids <- geo_point(crds = cntrds, window = extent)

    offset <- 45
    vertices <- 4

    radius <- width/2
    radius <- sqrt(radius**2 + radius**2)

  } else if(pattern == "hexagonal"){
    # https://www.redblobgames.com/grids/hexagons/

    offset <- 0
    nPoints <- 7

    inRadius <- width/2
    circumRadius <- 2/sqrt(3) * inRadius
    radius <- circumRadius

    xOffset <- inRadius*2*offset*-1
    yOffset <- 2*circumRadius*offset*-1

    # determine centroids
    xC1 <- seq(min(extent$x) - 2*inRadius - xOffset, max(extent$x) + 2*inRadius + xOffset, by = inRadius*2)
    xC2 <- seq(min(extent$x) - inRadius - xOffset, max(extent$x) + 3*inRadius + xOffset, by = inRadius*2)
    yC1 <- seq(min(extent$y) - yOffset, max(extent$y) + circumRadius + yOffset, by = 3*circumRadius)
    yC2 <- seq(min(extent$y) + 3/2*circumRadius - yOffset, max(extent$y) + 3/2*circumRadius + yOffset, by = 3*circumRadius)

    cntrds <- tibble(x = c(rep(xC1, times = length(yC1)), rep(xC2, times = length(yC2))),
                     y = c(rep(yC1, each = length(xC1)), rep(yC2, each = length(xC2))))
    targetCentroids <- geo_point(crds = cntrds, window = extent)

    offset <- 30
    vertices <- 6

  } else if(pattern == "triangular"){
    stop("triangular tiling is not yet supported.")
  }

  rotation <- offset
  angle <- 360/vertices
  angles <- seq(from = 0, to = 360-angle, by = angle) - rotation

  relX <- round(radius*cos(.rad(c(angles, angles[1]))), 14)
  relY <- round(radius*sin(.rad(c(angles, angles[1]))), 14)

  if(!centroids){
    nodes <- cx <- cy <- fids <- NULL
    for(i in seq_along(targetCentroids@geometry$fid)){
      cx <- c(cx, targetCentroids@geometry$x[i] + relX)
      cy <- c(cy, targetCentroids@geometry$y[i] + relY)
      fids <- c(fids, rep(i, nPoints))
    }

    nodes <- tibble(x = cx,
                    y = cy,
                    fid = fids)
    theType <- "polygon"
  } else{
    nodes <- tibble(x = cntrds$x,
                    y = cntrds$y,
                    fid = cntrds$fid)
    theType <- "point"
  }

  # nodes$incl <- as.logical(pointInPolyCpp(vert = as.matrix(nodes[c("x", "y")]),
  #                                         geom = as.matrix(extent[c("x", "y")]),
  #                                         invert = FALSE))

  # retain <- NULL
  # for(i in unique(nodes$fid)){
  #   temp <- nodes[nodes$fid == i,]
  #   if(any(temp$incl)){
  #     if(is.null(retain$fid)){
  #       temp$fid <- 1
  #     } else {
  #       temp$fid <- max(retain$fid) + 1
  #     }
  #     retain <- rbind(retain, temp)
  #   }
  # }
  thePoints <- tibble(fid = nodes$fid, x = nodes$x, y = nodes$y)
  theFeatures <- tibble(fid = unique(nodes$fid), gid = unique(nodes$fid))
  theGroups <- tibble(gid = unique(nodes$fid))

  newWindow <- tibble(x = c(min(thePoints$x), max(thePoints$x)),
                      y = c(min(thePoints$y), max(thePoints$y)))

  theData <- list(features = theFeatures, groups = theGroups)

  theTiles <- new(Class = "geom",
                  label = "polygon_geom",
                  type = theType,
                  geometry = thePoints,
                  data = theData,
                  window = newWindow,
                  crs = NA_character_,
                  provenance = list(paste0("tiled geometry of type '", theType, "' was created.")))

  invisible(theTiles)
}