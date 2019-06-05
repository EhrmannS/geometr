#' Create a regular tiling geometry
#'
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @param window [\code{data.frame(1)}]\cr the origin (lower left corner) and
#'   the maximum value (upper right corner) of the tiling.
#' @param width [\code{numeric(1)}]\cr the width of a tile.
#' @param offset [\code{numeric(1)}]\cr fraction of \code{width}, by which the
#'   tiles will be offset. Can go from \code{-1} to \code{+1}.
#' @param pattern [\code{character(1)}]\cr pattern of the tiling. Possible
#'   options are \code{"squared"} (default) or \code{"hexagonal"}.
#' @param rotation [\code{integerish(1)}]\cr
#' @param centroids [\code{logical(1)}]\cr should the centroids of the tiling be
#'   returned (\code{TRUE}) or should the tiling be returned (\code{FALSE},
#'   default)?
#' @details When deriving a regular tiling for a prescribed window, there is
#'   only a limited set of legal combinations of cells in x and y dimension. For
#'   instance, a window of 100 by 100 can't comprise 10 by 5 squares of
#'   side-length 10, because then the y-dimension wouldn't be fully covered. The
#'   same is true for hexagonal and triangular tilings. In case it is only
#'   properly clear how many tiles there should be in one dimension, but not the
#'   other, this can be specified by setting one of the cell counts to
#'   \code{NA}, such as \code{cells = c(NA, 18)}.
#' @return An invisible \code{geom}.
#' @family tilings
#' @examples
#' # create a squared tiling
#' aWindow <- data.frame(x = c(-180, 180),
#'                       y = c(-60, 80))
#' tiles <- gs_tiles(window = aWindow, width = 10)
#'
#' # create a hexagonal tiling for a geom
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#' comb <- gs_tiles(anchor = aGeom, width = 8, pattern = "hexagonal", rotation = 41)
#' @importFrom checkmate testDataFrame assertNames testClass testIntegerish
#'   assertDataFrame assertNames assertCharacter assertSubset assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows group_by summarise left_join select mutate
#' @export

gs_tiles <- function(anchor = NULL, window = NULL, width = NULL, offset = NULL,
                     pattern = "squared", rotation = 0, centroids = FALSE, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor, ...)
  window <- .testWindow(x = window, ...)

  if(is.null(anchor) & is.null(window)){
    stop("please provide either 'anchor' or 'window'.")
  }


  if(is.null(anchor)){
    anchor <- gs_rectangle(anchor = window)
  }

  assertIntegerish(width, len = 1)
  assertChoice(x = pattern, choices = c("squared", "hexagonal", "triangular"))
  assertNumeric(x = rotation, lower = 0, upper = 360, len = 1)
  assertLogical(centroids)

  targetRotation <- rotation
  if(targetRotation != 0){
    coords <- getSubset(anchor, !!(!duplicated(anchor@vert[c("x", "y")])), slot = "vert")
    originCentroid <- c(mean(x = coords@vert$x), mean(x = coords@vert$y))
    anchor <- gt_rotate(geom = anchor, about = originCentroid, angle = -targetRotation)
  }

  # set pattern specific properties
  if(pattern == "squared"){

    if(is.null(offset)){
      offset <- 0.5
    }
    offset <- width*offset*-1

    xRange <- max(anchor@vert$x) - min(anchor@vert$x)
    yRange <- max(anchor@vert$y) - min(anchor@vert$y)

    # determine centroids
    xCentroids <- seq(min(anchor@vert$x) - offset, max(anchor@vert$x) + offset, width)
    yCentroids <- seq(min(anchor@vert$y) - offset, max(anchor@vert$y) + offset, width)
    cntrds <- tibble(fid = seq(1:(length(xCentroids)*length(yCentroids))),
                     x = rep(xCentroids, times = length(yCentroids)),
                     y = rep(yCentroids, each = length(xCentroids)))
    targetCentroids <- gs_point(anchor = cntrds, window = anchor@window)

    offset <- 45
    vertices <- 4

    radius <- width/2
    radius <- sqrt(radius**2 + radius**2)

  } else if(pattern == "hexagonal"){
    # https://www.redblobgames.com/grids/hexagons/

    inRadius <- width/2
    circumRadius <- 2/sqrt(3) * inRadius
    radius <- circumRadius

    if(is.null(offset)){
      offset <- 0
    }
    xOffset <- inRadius*2*offset*-1
    yOffset <- 2*circumRadius*offset*-1

    xRange <- max(anchor@vert$x) - min(anchor@vert$x)
    yRange <- max(anchor@vert$y) - min(anchor@vert$y)

    # determine centroids
    xC1 <- seq(min(anchor@vert$x) - 2*inRadius - xOffset, max(anchor@vert$x) + 2*inRadius + xOffset, by = inRadius*2)
    xC2 <- seq(min(anchor@vert$x) - inRadius - xOffset, max(anchor@vert$x) + 3*inRadius + xOffset, by = inRadius*2)
    yC1 <- seq(min(anchor@vert$y) - yOffset, max(anchor@vert$y) + circumRadius + yOffset, by = 3*circumRadius)
    yC2 <- seq(min(anchor@vert$y) + 3/2*circumRadius - yOffset, max(anchor@vert$y) + 3/2*circumRadius + yOffset, by = 3*circumRadius)

    cntrds <- tibble(fid = seq(1:(length(yC1)*length(xC1) + length(yC2)*length(xC2))),
                     x = c(rep(xC1, times = length(yC1)), rep(xC2, times = length(yC2))),
                     y = c(rep(yC1, each = length(xC1)), rep(yC2, each = length(xC2))))
    targetCentroids <- gs_point(anchor = cntrds, window = window)

    offset <- 30
    vertices <- 6

  } else if(pattern == "triangular"){
    stop("triangular tiling is not yet supported.")
  }

  rotation <- offset
  angle <- 360/vertices
  angles <- seq(from = 0, to = 360-angle, by = angle) - rotation

  relX <- radius*cos(.rad(angles))
  relY <- radius*sin(.rad(angles))

  if(!centroids){
    nodes <- NULL
    for(i in seq_along(targetCentroids@vert$fid)){
      cx <- targetCentroids@vert$x[i] + relX
      cx <- c(cx, cx[1])
      cy <- targetCentroids@vert$y[i] + relY
      cy <- c(cy, cy[1])
      theNodes <- tibble(x = cx,
                         y = cy,
                         fid = i)
      nodes <- bind_rows(nodes, theNodes)
    }
    theType <- "polygon"
  } else{
    nodes <- tibble(x = cntrds$x,
                    y = cntrds$y,
                    fid = cntrds$fid)
    theType <- "point"
  }

  theTiles <- new(Class = "geom",
                  type = theType,
                  vert = nodes,
                  feat = tibble(fid = unique(nodes$fid), gid = unique(nodes$fid)),
                  group = tibble(gid = unique(nodes$fid)),
                  window = tibble(x = c(min(window$x), max(window$x), max(window$x), min(window$x), min(window$x)),
                                  y = c(min(window$y), min(window$y), max(window$y), max(window$y), min(window$y))),
                  scale = "absolute",
                  crs = NA_character_,
                  history = list(paste0("tiled geometry of type '", theType, "' was created.")))

  theTiles@vert$include <- as.logical(pointInGeomC(vert = as.matrix(theTiles@vert[c("x", "y")]),
                                                   geom = as.matrix(anchor@vert[c("x", "y")]),
                                                   invert = FALSE))

  sbst <- group_by(theTiles@vert, fid)
  sbst <- summarise(sbst, sub = any(include == TRUE))
  # theTiles <- setTable(x = theTiles, table = sbst, regroup = FALSE)

  theTiles <- getSubset(x = theTiles, !!sbst$sub, slot = "feat")

  # reconstruct IDs
  newIDs <- tibble(fid = theTiles@feat$fid, new = seq_along(fid))
  theTiles@vert <- left_join(theTiles@vert, newIDs, by = "fid")
  theTiles@vert <- select(theTiles@vert, "x", "y", fid = new)
  theTiles@feat <- left_join(theTiles@feat, newIDs, by = "fid")
  theTiles@feat <- mutate(theTiles@feat, fid = new, gid = fid)
  theTiles@feat <- select(theTiles@feat, fid, gid)

  if(targetRotation != 0){
    theTiles <- gt_rotate(geom = theTiles, about = originCentroid, angle = targetRotation, update = FALSE)
  }

  invisible(theTiles)
}