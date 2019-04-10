#' Create a regular tiling geometry
#'
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @param window [\code{data.frame(1)}]\cr the origin (lower left corner) and
#'   the maximum value (upper right corner) of the tiling.
#' @param diameter [\code{numeric(1)}]\cr the (outer) diameter of each tile.
#' @param cells [\code{integerish(2)}]\cr number of geometries to create. If two
#'   values (including \code{NA}) are given, they are for x and y-dimension, if
#'   only one value is given, it denotes the overall amount of cells.
#' @param pattern [\code{character(1)}]\cr pattern of the tiling. Possible
#'   options are \code{"squared"} (default) or \code{"hexagonal"}.
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
#' #aWindow <- data.frame(x = c(-180, 180),
#' #                       y = c(-60, 80))
#' #tiles <- gs_tiles(window = aWindow, diameter = 10)
#'
#' # create a hexagonal tiling spanning the whole world
#' theWorld <- data.frame(x = c(-180, 180),
#'                         y = c(-90, 90))
#' #comb <- gs_tiles(window = theWorld, diameter = 10, pattern = "hexagonal")
#' @importFrom checkmate testDataFrame assertNames testClass testIntegerish
#'   assertDataFrame assertNames assertCharacter assertSubset assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export

gs_tiles <- function(anchor = NULL, window = NULL, diameter = NULL, cells = NULL,
                     pattern = "squared", centroids = FALSE){

  # if(tiling == "rectangular"){
  #
  #   xDist <- (abs(min(window$x)) + abs(max(window$x)))/cells[1]
  #   yDist <- (abs(min(window$y)) + abs(max(window$y)))/cells[2]
  #   if(round(xDist, 3) != round(yDist, 3)){
  #     stop("a tiling of ", cells[1], " by ", cells[2], " cells would yield irregular cells with the given 'window'.\n Please adapt either 'cells' or 'window'.")
  #   }
  #
  #   # determine centroids
  #   xCentroids <- seq(min(window$x) + xDist/2, max(window$x), xDist)
  #   yCentroids <- seq(min(window$y) + yDist/2, max(window$y), yDist)
  #   cntrds <- tibble(fid = seq(1:(cells[1]*cells[2])),
  #                    x = rep(xCentroids, times = length(yCentroids)),
  #                    y = rep(yCentroids, each = length(xCentroids)))
  #
  #   angle <- 360/4
  #   angles <- seq(from = 45, to = 360-angle+45, by = angle)
  #   radius <- sqrt(xDist**2 + yDist**2)/2
  #
  # } else if(tiling == "hexagonal"){
  #
  #   height <- (abs(min(window$y)) + abs(max(window$y)))/cells[2]
  #   width <- 2 * (height / sqrt(3))
  #
  #   # determine centroids
  #   xC1 <- seq(min(window$x), max(window$x) + width/2, by = 3/2*width)
  #   xC2 <- seq(min(window$x) + 3/4*width, max(window$x), by = 3/2*width)
  #   if(length(xC1) > cells[1]/2+1){
  #     xC1 <- xC1[c(1:(cells[1]/2+1))]
  #     xC2 <- xC2[c(1:(cells[1]/2))]
  #   }
  #   yC1 <- seq(min(window$y), max(window$y), by = height)
  #   yC2 <- seq(min(window$y) + height/2, max(window$y), by = height)
  #
  #   cntrds <- tibble(fid = seq(1:(length(yC1)*length(xC1) + length(yC2)*length(xC2))),
  #                    x = c(rep(xC1, times = length(yC1)), rep(xC2, times = length(yC2))),
  #                    y = c(rep(yC1, each = length(xC1)), rep(yC2, each = length(xC2))))
  #
  #   angle <- 360/6
  #   angles <- seq(from = 0, to = 360-angle, by = angle)
  #   radius <- width/2
  #
  # }
  #
  # if(!centroids){
  #   nodes <- NULL
  #   for(i in seq_along(cntrds$fid)){
  #     cx <- round(cntrds$x[i] + radius*cos(rad(angles)))
  #     cy <- round(cntrds$y[i] + radius*sin(rad(angles)))
  #     theNodes <- tibble(fid = i, vid = 1:length(angles), x = cx, y = cy)
  #     nodes <- bind_rows(nodes, theNodes)
  #   }
  #   theType <- "polygon"
  # } else{
  #   nodes <- cntrds
  #   theType <- "point"
  # }
  #
  # theTiles <- new(Class = "geom",
  #                 type = theType,
  #                 coords = nodes,
  #                 attr = tibble(fid = unique(nodes$fid), n = 1),
  #                 window = window,
  #                 scale = "absolute",
  #                 crs = as.character(projection),
  #                 history = list(paste0("tiled geometry of type '", theType, "' was created.")))

  # check arguments
  anchorIsDF <- testDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
  if(anchorIsDF){
    # colnames(anchor) <- tolower(colnames(anchor))
    # assertNames(names(anchor), must.include = c("x", "y"), subset.of = c( "fid", "vid", "x", "y"))
    # if(!"vid" %in% names(anchor)){
    #   anchor <- bind_cols(vid = rep(1, times = length(anchor$x)), anchor)
    # }
    # if(!"fid" %in% names(anchor)){
    #   anchor <- bind_cols(fid = seq_along(anchor$x), anchor)
    # }
    # features <- length(unique(anchor$fid))
  }
  anchorIsGeom <- testClass(anchor, classes = "geom")
  if(anchorIsGeom){
    # # fid and vid values need to be transformed
    # if(anchor@type != "point"){
    #   anchor@vert$fid <- seq_along(anchor@vert$fid)
    #   anchor@vert$vid <- rep(1, times = length(anchor@vert$vid))
    # }
    # features <- length(unique(anchor@vert$fid))
  }
  windowExists <- !testNull(window)
  if(windowExists){
    assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
    colnames(window) <- tolower(colnames(window))
    assertNames(names(window), must.include = c("x", "y"))
  }

  assertIntegerish(diameter, len = 1)
  assertIntegerish(cells, len = 2, any.missing = FALSE)
  assertChoice(x = pattern, choices = c("squared", "hexagonal", "triangular"))
  # assertChoice(x = align, choices = c("horizontal", "vertical"))
  # assertIntegerish(x = rotation, lower = 0, upper = 360, len = 1)
  assertLogical(centroids)


  xCells <- (abs(min(window$x)) + abs(max(window$x)))/diameter
  yCells <- (abs(min(window$y)) + abs(max(window$y)))/diameter

  # determine centroids
  xCentroids <- seq(min(window$x) + diameter/2, max(window$x), diameter)
  yCentroids <- seq(min(window$y) + diameter/2, max(window$y), diameter)
  cntrds <- tibble(fid = seq(1:(xCells*yCells)),
                   x = rep(xCentroids, times = length(yCentroids)),
                   y = rep(yCentroids, each = length(xCentroids)))
  points <- gs_point(anchor = cntrds, window = window)

  # set pattern specific properties
  if(pattern == "squared"){
    vertices <- 4
    offset <- 45

    newPoints <- points
    # # radius <- sqrt(diameter**2 + diameter**2)/2
    # radius <- diameter/2

  } else if(pattern == "hexagonal"){
    vertices <- 6
    offset <- 30

    # https://www.redblobgames.com/grids/hexagons/

    radius <- diameter/2
    radius <- 2/sqrt(3) * radius

    # gt_group to group 'fid' so that always two rows are grouped, then skew
    newPoints <- gt_skew(geom = points, x = 0.5)
    # then group so that all features are at 'fid = 1' and stretch
    newPoints <- gt_stretch(geom = newPoints, y = sqrt(3)/2)
    # if(align){
    #   width <- abs(min(window$x)) + abs(max(window$x))
    #   xCells <- floor(width/(inradius*2))
    #   inradius <- (width/xCells)/2
    #   radius <- inradius*2/sqrt(3)
    # }

    # # determine centroids
    # xC1 <- seq(min(window$x), max(window$x), by = inradius*2)
    # xC2 <- seq(min(window$x) + inradius, max(window$x), by = inradius*2)
    # yC1 <- seq(min(window$y), max(window$y) + radius, by = 3/2*radius*2)
    # yC2 <- seq(min(window$y) + 3/2*radius, max(window$y), by = 3/2*radius*2)
    #
    # cntrds <- tibble(fid = seq(1:(length(yC1)*length(xC1) + length(yC2)*length(xC2))),
    #                  x = c(rep(xC1, times = length(yC1)), rep(xC2, times = length(yC2))),
    #                  y = c(rep(yC1, each = length(xC1)), rep(yC2, each = length(xC2))))

  } else if(pattern == "triangular"){
    vertices <- 3
    offset <- 60

    # newPoints <-
  }

  rotation <- offset
  angle <- 360/vertices
  angles <- seq(from = 0, to = 360-angle, by = angle) - rotation

  relX <- radius*cos(rad(angles))
  relY <- radius*sin(rad(angles))

  # make the window
  # window <- tibble(x = rep(window$x, each = 2),
  #                  y = c(window$y, rev(window$y)))

  if(!centroids){
    nodes <- NULL
    for(i in seq_along(newPoints@vert$fid)){
      cx <- newPoints@vert$x[i] + relX
      cy <- newPoints@vert$y[i] + relY
      theNodes <- tibble(fid = i, vid = 1:length(angles), x = cx, y = cy)
      # remove nodes that are not valid
      # theNodes$x[theNodes$x < min(window$x)] <- min(window$x)
      # theNodes$x[theNodes$x > max(window$x)] <- max(window$x)
      # theNodes$y[theNodes$y < min(window$y)] <- min(window$y)
      # theNodes$y[theNodes$y > max(window$y)] <- max(window$y)
      # theNodes <- theNodes[!duplicated(theNodes[c("x", "y")]),]
      nodes <- bind_rows(nodes, theNodes)
    }
    theType <- "polygon"
  } else{
    nodes <- cntrds
    theType <- "point"
  }

  theTiles <- new(Class = "geom",
                  type = theType,
                  vert = nodes,
                  attr = tibble(fid = unique(nodes$fid), gid = unique(nodes$fid)),
                  window = window,
                  scale = "absolute",
                  crs = NA_character_,
                  history = list(paste0("tiled geometry of type '", theType, "' was created.")))

  invisible(theTiles)
}