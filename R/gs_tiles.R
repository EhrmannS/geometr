#' Create a regular tiling geometry
#'
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @param window [\code{data.frame(1)}]\cr the origin (lower left corner) and
#'   the maximum value (upper right corner) of the tiling.
#' @param width [\code{numeric(1)}]\cr the width of a tile.
#' @param cells [\code{integerish(2)}]\cr number of geometries to create. If two
#'   values (including \code{NA}) are given, they are for x and y-dimension, if
#'   only one value is given, it denotes the overall amount of cells.
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
#' # create a hexagonal tiling spanning the whole world
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#' comb <- gs_tiles(anchor = aGeom, width = 8, pattern = "hexagonal", rotation = 45)
#' @importFrom checkmate testDataFrame assertNames testClass testIntegerish
#'   assertDataFrame assertNames assertCharacter assertSubset assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows group_by summarise
#' @export

gs_tiles <- function(anchor = NULL, window = NULL, width = NULL, cells = NULL,
                     pattern = "squared", rotation = 0, centroids = FALSE){

  # check arguments
  anchorIsDF <- testDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
  anchorIsGeom <- testClass(anchor, classes = "geom")
  windowExists <- !testNull(window)
  assert(anchorIsDF, anchorIsGeom, windowExists)

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
  if(anchorIsGeom){
    # # fid and vid values need to be transformed
    # if(anchor@type != "point"){
    #   anchor@vert$fid <- seq_along(anchor@vert$fid)
    #   anchor@vert$vid <- rep(1, times = length(anchor@vert$vid))
    # }
    # features <- length(unique(anchor@vert$fid))
  }
  if(windowExists){
    assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
    colnames(window) <- tolower(colnames(window))
    assertNames(names(window), must.include = c("x", "y"))

    if(dim(window)[1] != 4){
      window <- tibble(x = rep(c(min(window$x), max(window$x)), each = 2), y = c(min(window$y), max(window$y), max(window$y), min(window$y)))
    }
  } else {
    window <- tibble(x = c(min(anchor@window$x), max(anchor@window$x)),
                     y = c(min(anchor@window$y), max(anchor@window$y)))
  }

  if(!anchorIsDF & !anchorIsGeom){
    anchor <- gs_rectangle(anchor = window)
  }

  assertIntegerish(width, len = 1)
  # assertIntegerish(cells, len = 2, any.missing = FALSE)
  assertChoice(x = pattern, choices = c("squared", "hexagonal", "triangular"))
  assertNumeric(x = rotation, lower = 0, upper = 360, len = 1)
  assertLogical(centroids)

  targetRotation <- rotation
  if(targetRotation != 0){
    originCentroid <- c(mean(x = anchor@vert$x), mean(x = anchor@vert$y))
    anchor <- gt_rotate(geom = anchor, about = originCentroid, angle = -targetRotation)
  }

  # set pattern specific properties
  if(pattern == "squared"){

    xRange <- max(anchor@vert$x) - min(anchor@vert$x)
    yRange <- max(anchor@vert$y) - min(anchor@vert$y)

    # cols <- ceiling(xRange / width)
    # rows <- ceiling(yRange / width)

    # determine centroids
    xCentroids <- seq(min(anchor@vert$x) + width/2, max(anchor@vert$x), width)
    yCentroids <- seq(min(anchor@vert$y) + width/2, max(anchor@vert$y), width)
    cntrds <- tibble(fid = seq(1:(length(xCentroids)*length(yCentroids))),
                     x = rep(xCentroids, times = length(yCentroids)),
                     y = rep(yCentroids, each = length(xCentroids)))
    targetCentroids <- gs_point(anchor = cntrds, window = window)

    offset <- 45
    vertices <- 4

    radius <- width/2
    radius <- sqrt(radius**2 + radius**2)

  } else if(pattern == "hexagonal"){
    # https://www.redblobgames.com/grids/hexagons/

    xRange <- max(anchor@vert$x) - min(anchor@vert$x)
    yRange <- max(anchor@vert$y) - min(anchor@vert$y)

    inRadius <- width/2
    circumRadius <- 2/sqrt(3) * inRadius
    radius <- circumRadius

    # cols <- ceiling(xRange / (inRadius*2))
    # rows <- ceiling(yRange / (3/2*circumRadius) / 2)

    # determine centroids
    xC1 <- seq(min(anchor@vert$x) - 2*inRadius, max(anchor@vert$x) + 2*inRadius, by = inRadius*2)
    xC2 <- seq(min(anchor@vert$x) - inRadius, max(anchor@vert$x) + 3*inRadius, by = inRadius*2)
    yC1 <- seq(min(anchor@vert$y), max(anchor@vert$y) + circumRadius, by = 3*circumRadius)
    yC2 <- seq(min(anchor@vert$y) + 3/2*circumRadius, max(anchor@vert$y) + 3/2*circumRadius, by = 3*circumRadius)

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

  relX <- radius*cos(rad(angles))
  relY <- radius*sin(rad(angles))

  if(!centroids){
    nodes <- NULL
    for(i in seq_along(targetCentroids@vert$fid)){
      cx <- targetCentroids@vert$x[i] + relX
      cy <- targetCentroids@vert$y[i] + relY
      theNodes <- tibble(fid = i, vid = 1:length(angles), x = cx, y = cy)
      nodes <- bind_rows(nodes, theNodes)
    }
    theType <- "polygon"
  } else{
    nodes <- tibble(fid = cntrds$fid, vid = 1, x = cntrds$x, y = cntrds$y)
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

  theTiles@vert$include <- as.logical(pointInGeomC(vert = as.matrix(theTiles@vert[c("x", "y")]),
                                                   geom = as.matrix(anchor@vert[c("x", "y")]),
                                                   invert = FALSE))

  sbst <- group_by(theTiles@vert, fid)
  sbst <- summarise(sbst, sub = any(include == TRUE))
  theTiles <- getSubset(x = theTiles, !!sbst$sub, slot = "table")

  if(targetRotation != 0){
    theTiles <- gt_rotate(geom = theTiles, about = originCentroid, angle = targetRotation)
  }

  invisible(theTiles)
}