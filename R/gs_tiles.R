#' Create a regular tiling
#'
#' @param window [\code{data.frame(1)}]\cr the origin (lower left corner) and
#'   the maximum value (upper right corner) of the tiling.
#' @param cells [\code{integerish(2)}]\cr the number of cells in x and/or y
#'   dimension.
#' @param size [\code{numeric(1)}]\cr the diameter of each tile.
#' @param tiling [\code{character(1)}]\cr pattern of the tiling. Possible
#'   options are \code{"squared"} (default) or \code{"hexagonal"}.
#' @param align [\code{logical(1)}]\cr should the tiles be aligned so that they
#'   converge to the same area at the boundary in x-dimension (\code{TRUE}), or
#'   should they converge in y-dimension (\code{FALSE}, default).
#' @param rotation [\code{numeric(1)}]\cr the degree by which the tiles should
#'   be rotated.
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
#' @examples
#' # create a squared tiling
#' #aWindow <- data.frame(x = c(-180, 180),
#'#                       y = c(-60, 80))
#' #tiles <- gs_tiles(window = aWindow, size = 10)
#'
#' # create a hexagonal tiling spanning the whole world
#' #theWorld <- data.frame(x = c(-180, 180),
#' #                        y = c(-90, 90))
#' #comb <- gs_tiles(window = theWorld, size = 10, tiling = "hexagonal")
#' @family tilings
#' @importFrom checkmate testDataFrame assertNames testClass testIntegerish
#'   assertDataFrame assertNames assertCharacter assertSubset assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export

gs_tiles <- function(anchor = NULL, window = NULL, size = NULL, features = NULL,
                     tiling = "squared", align = "vertical", rotation = 0, centroids = FALSE){

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

  assertIntegerish(size, len = 1)
  assertChoice(x = tiling, choices = c("squared", "hexagonal", "triangular"))
  assertChoice(x = align, choices = c("horizontal", "vertical"))
  assertIntegerish(x = rotation, lower = 0, upper = 360, len = 1)
  assertLogical(centroids)

  if(tiling == "squared"){

    angle <- 360/4
    angles <- seq(from = 45, to = 360-angle+45, by = angle) - rotation
    radius <- sqrt(size**2 + size**2)/2

    xCells <- (abs(min(window$x)) + abs(max(window$x)))/size
    yCells <- (abs(min(window$y)) + abs(max(window$y)))/size

    # determine centroids
    xCentroids <- seq(min(window$x) + size/2, max(window$x), size)
    yCentroids <- seq(min(window$y) + size/2, max(window$y), size)
    cntrds <- tibble(fid = seq(1:(xCells*yCells)),
                     x = rep(xCentroids, times = length(yCentroids)),
                     y = rep(yCentroids, each = length(xCentroids)))

  } else if(tiling == "hexagonal"){

    radius <- size/2
    inradius <- sqrt(3)/2 * radius
    if(align){
      width <- abs(min(window$x)) + abs(max(window$x))
      xCells <- floor(width/(inradius*2))
      inradius <- (width/xCells)/2
      radius <- inradius*2/sqrt(3)
    }
    # without 'align' the right side still needs improvement, as some of the vertices get cut off insensibly, plus there is some coverage missing

    angle <- 360/6
    angles <- seq(from = 30, to = 360-angle+30, by = angle)

    # determine centroids
    xC1 <- seq(min(window$x), max(window$x), by = inradius*2)
    xC2 <- seq(min(window$x) + inradius, max(window$x), by = inradius*2)
    yC1 <- seq(min(window$y), max(window$y) + radius, by = 3/2*radius*2)
    yC2 <- seq(min(window$y) + 3/2*radius, max(window$y), by = 3/2*radius*2)

    cntrds <- tibble(fid = seq(1:(length(yC1)*length(xC1) + length(yC2)*length(xC2))),
                     x = c(rep(xC1, times = length(yC1)), rep(xC2, times = length(yC2))),
                     y = c(rep(yC1, each = length(xC1)), rep(yC2, each = length(xC2))))
    # visualise(geom = setWindow(x = gs_point(anchor = cntrds), to = window))

  }

  window <- tibble(x = rep(window$x, each = 2),
                   y = c(window$y, rev(window$y)))

  if(!centroids){
    nodes <- NULL
    for(i in seq_along(cntrds$fid)){
      cx <- cntrds$x[i] + radius*cos(rad(angles))
      cy <- cntrds$y[i] + radius*sin(rad(angles))
      theNodes <- tibble(fid = i, vid = 1:length(angles), x = cx, y = cy)
      # remove nodes that are not valid
      # theNodes$x[theNodes$x < min(window$x)] <- min(window$x)
      # theNodes$x[theNodes$x > max(window$x)] <- max(window$x)
      # theNodes$y[theNodes$y < min(window$y)] <- min(window$y)
      # theNodes$y[theNodes$y > max(window$y)] <- max(window$y)
      theNodes <- theNodes[!duplicated(theNodes[c("x", "y")]),]
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
  # visualise(geom = theTiles, new = F)

  invisible(theTiles)
}